(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation
   files (the "Software"), to deal in the Software without
   restriction, including without limitation the rights to use, copy,
   modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

(** Assembly sanitisation *)

open Core

(** [Hook] is an interface for language-specific hooks into the
    sanitisation process. *)
module type Hook = sig
  module L : Language.S
  module Ctx : Sanitiser_ctx.S with module Lang = L

  (** [on_program] is a hook mapped over each the program as a
      whole. *)
  val on_program : L.Statement.t list -> (L.Statement.t list) Ctx.t

  (** [on_statement] is a hook mapped over each statement in the
      program. *)
  val on_statement : L.Statement.t -> L.Statement.t Ctx.t

  (** [on_instruction] is a hook mapped over each instruction in the
      program. *)
  val on_instruction : L.Instruction.t -> L.Instruction.t Ctx.t

  (** [on_location] is a hook mapped over each location in the
      program. *)
  val on_location : L.Location.t -> L.Location.t Ctx.t
end

(** [Basic] describes the base functionality we need to supply to a
    sanitiser.

    It includes both a [Hook] that inserts language-specific
    sanitisers into the sanitiser, and a description of how we split
    the incoming assembly into programs and traverse over them.

    The dependency on a separate [Program_container] lets us adapt
    the sanitiser to single-program and multi-program contexts
    more easily. *)
module type Basic = sig
  include Hook

  (** [Program_container] describes the monadic-fold-mappable
     container that we'll be using to hold the one or more programs
     we're sanitising. *)
  module Program_container : Utils.Fold_map.Container1

  (** [split] splits an assembly script up into the one or more
      programs we'll be sanitising. *)
  val split
    :  L.Statement.t list
    -> L.Statement.t list Program_container.t Or_error.t
end

(** [S] is the interface to a fully-built sanitiser. *)
module type S = sig
  module Warn : Sanitiser_ctx.Warn

  type statement
  type sym

  (** [Program_container] describes the container that the sanitised
     program or programs are held in. *)
  module Program_container : Container.S1

  module Output : sig
    (** [t] is the type of (successful) sanitiser output. *)
    type t

    val result : t -> statement list Program_container.t

    val warnings : t -> Warn.t list

    val redirects
      : t
      -> (sym, sym)
        List.Assoc.t
  end

  (** [sanitise ?passes ?symbols stms] sanitises a statement list [stms],
      returning one or more program lists with various litmus-unfriendly
      elements removed or simplified.

      If the implementation of the sanitiser outputs multiple programs,
      [stms] will be split along its program boundaries first.

      Optional argument 'passes' controls the sanitisation passes
      used; the default is [Pass.all].

      Optional argument 'symbols' gives a list of concrete symbols
      that the sanitiser should track throughout the sanitisation
      process. *)
  val sanitise
    :  ?passes:Sanitiser_pass.Set.t
    -> ?symbols:sym list
    -> statement list
    -> Output.t Or_error.t
end

(** [Sanitiser] describes the interface we export in
    [Sanitiser.mli]. *)
module type Sanitiser = sig
  module type Hook = Hook
  module type Basic = Basic
  module type S = S

  (** [Make_null_hook] makes a [Hook] that does nothing. *)
  module Make_null_hook
    : functor (L : Language.S)
      -> Hook with module L = L

  (** [Make] implements the assembly sanitiser for a given [Basic]. *)
  module Make :
    functor (B : Basic)
      -> S with type statement = B.L.Statement.t
            and type sym = B.L.Symbol.t
            and type 'a Program_container.t = 'a B.Program_container.t
  ;;

  (** [Make_single] implements the assembly sanitiser for a given
     [Hook], performing no program splitting and returning the
     sanitised assembly back as one program. *)
  module Make_single :
    functor (H : Hook)
      -> S with type statement = H.L.Statement.t
            and type sym = H.L.Symbol.t
            and type 'a Program_container.t = 'a
  ;;

  (** [Make_multi] implements the assembly sanitiser for a given
     [Hook], treating the incoming assembly as holding multiple
     label-delimited programs and splitting them accordingly. *)
  module Make_multi :
    functor (H : Hook)
      -> S with type statement = H.L.Statement.t
            and type sym = H.L.Symbol.t
            and type 'a Program_container.t = 'a list
  ;;
end
