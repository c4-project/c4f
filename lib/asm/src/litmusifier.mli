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

(** The litmusifier.

    The `act` litmusifier takes the result of a sanitiser run and converts
    it into a Litmus test.

    Because the output of a sanitiser run, and the format of the resulting
    litmus test, depends heavily on the assembly language being sanitised,
    the bulk of this module is in the {{!Make} Make} functor. *)

open Base

(** Output formats for the litmusifier. *)
module Format : sig
  (** [t] is an enumeration of output formats for litmus jobs. *)
  type t =
    | Full  (** Output a full, herd-compatible litmus test *)
    | Programs_only
        (** Only output the program tables (eg for comparison) *)
  [@@deriving equal]

  val default : t
  (** [default] gets the default output format. *)
end

(** Configuration for a litmusifier run. *)
module Config : sig
  type t [@@deriving equal]

  (** {3 Constructors} *)

  val make : ?format:Format.t -> ?aux:Act_delitmus.Aux.t -> unit -> t
  (** [make ?format ?aux ()] builds a litmusifier config with the given
      parameters. *)

  val default : unit -> t
  (** [default ()] gets the default Litmus job configuration. *)

  (** {3 Accessors} *)

  val aux : t -> Act_delitmus.Aux.t
  (** [aux config] gets the auxiliary information, passed into [config],
      about the original C test whose assembly derivative is being
      processed. *)

  val format : t -> Format.t
  (** [format config] gets the format stored in [config]. *)

  (** {3 Modifiers} *)

  val transform :
       t
    -> format:(Format.t -> Format.t Or_error.t)
    -> aux:(Act_delitmus.Aux.t -> Act_delitmus.Aux.t Or_error.t)
    -> t Or_error.t
  (** [transform config ~format ~postcondition ~c_variables] transforms
      [config] with the given functions. It fails if any of the transformers
      fail. *)
end

module Make (B : Runner_intf.Basic) :
  Litmusifier_intf.S
    with type config = Config.t
     and type fmt = Format.t
     and type program =
          ( B.Src_lang.Element.t
          , B.Src_lang.Program.t )
          Act_sanitiser.Output.Program.t
     and module Redirect := B.Src_lang.Symbol.R_map
