(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to permit
   persons to whom the Software is furnished to do so, subject to the
   following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
   NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
   DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
   OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
   USE OR OTHER DEALINGS IN THE SOFTWARE. *)

(** Assembly sanitisation *)

open Base
open Act_common

(** [Hook] is an interface for language-specific hooks into the sanitisation
    process. *)
module type Hook = sig
  include Sanitiser_base.Basic

  include
    Sanitiser_base.S_all
    with module Lang := Lang
     and module Ctx := Ctx
     and module Program_container := Program_container

  include
    Sanitiser_base.S_program
    with module Lang := Lang
     and module Ctx := Ctx
     and module Program_container := Program_container

  include
    Sanitiser_base.S_statement
    with module Lang := Lang
     and module Ctx := Ctx
     and module Program_container := Program_container

  include
    Sanitiser_base.S_instruction
    with module Lang := Lang
     and module Ctx := Ctx
     and module Program_container := Program_container

  include
    Sanitiser_base.S_location
    with module Lang := Lang
     and module Ctx := Ctx
     and module Program_container := Program_container
end

(** [Hook_maker] is the type of functors that generate hooks given a program
    container. *)
module type Hook_maker = functor (P : Travesty.Traversable.S1) -> Hook
                                                                  with module Program_container = P

(** [Basic] describes the base functionality we need to supply to a
    sanitiser.

    It includes both a [Hook] that inserts language-specific sanitisers into
    the sanitiser, and a description of how we split the incoming assembly
    into programs and traverse over them.

    The dependency on a separate [Program_container] lets us adapt the
    sanitiser to single-program and multi-program contexts more easily. *)
module type Basic = sig
  include Hook

  val split :
    Lang.Program.t -> Lang.Program.t Program_container.t Or_error.t
  (** [split] splits an assembly script up into the one or more programs
      we'll be sanitising. *)
end

(** [S] is the interface to a fully-built sanitiser. *)
module type S = sig
  (** [Lang] is the language over which we are sanitising. *)
  module Lang : Language.Definition.S

  module Warn : Sanitiser_warn.S with module Lang := Lang

  (** The type of symbol redirect maps this sanitiser outputs. *)
  module Redirect :
    Redirect_map.S
    with type sym := Lang.Symbol.t
     and type sym_set := Lang.Symbol.Set.t

  (** [Program_container] describes the container that the sanitised program
      or programs are held in. *)
  module Program_container : Container.S1

  module Output : sig
    (** [t] is the type of (successful) sanitiser output. *)
    type t

    (** [Program] is the abstract data type of a single program's sanitiser
        output. *)
    module Program : sig
      type t

      val listing : t -> Lang.Program.t
      (** [listing p] gets the final sanitised program listing. *)

      val warnings : t -> Warn.t list
      (** [warnings t] gets the warning list for this program. *)

      val symbol_table : t -> Abstract.Symbol.Table.t
      (** [symbol_table t] gets the program's final symbol table. *)
    end

    val programs : t -> Program.t Program_container.t
    (** [programs t] gets the final sanitised programs. *)

    val redirects : t -> Redirect.t
    (** [redirects t] gets the final mapping from C-level symbols to the
        symbols in [programs t]. *)
  end

  val sanitise :
       ?passes:Config.Sanitiser_pass.Set.t
    -> ?symbols:Lang.Symbol.t list
    -> Lang.Program.t
    -> Output.t Or_error.t
  (** [sanitise ?passes ?symbols stms] sanitises a statement list [stms],
      returning one or more program lists with various litmus-unfriendly
      elements removed or simplified.

      If the implementation of the sanitiser outputs multiple programs,
      [stms] will be split along its program boundaries first.

      Optional argument 'passes' controls the sanitisation passes used; the
      default is [Pass.all].

      Optional argument 'symbols' gives a list of concrete symbols that the
      sanitiser should track throughout the sanitisation process. *)
end
