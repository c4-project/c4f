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
open Act_common

(** @inline *)
include module type of Litmusifier_intf

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
  type 'const t [@@deriving equal, sexp]

  (** {3 Constructors} *)

  val make :
       ?format:Format.t
    -> ?postcondition:'const Act_litmus.Ast_base.Postcondition.t
    -> ?c_variables:C_variables.Map.t
    -> unit
    -> 'const t
  (** [make ?format ?postcondition ?c_variables ()] builds a [Litmus_config]
      with the given parameters. *)

  val default : unit -> 'a t
  (** [default ()] gets the default Litmus job configuration. *)

  (** {3 Accessors} *)

  val c_variables : _ t -> C_variables.Map.t option
  (** [c_variables config] gets the auxiliary C variable information passed
      into [config], if any. *)

  val format : _ t -> Format.t
  (** [format config] gets the format stored in [config]. *)

  (** {3 Modifiers} *)

  val transform :
       'a t
    -> format:(Format.t -> Format.t Or_error.t)
    -> postcondition:(   'a Act_litmus.Ast_base.Postcondition.t
                      -> 'b Act_litmus.Ast_base.Postcondition.t Or_error.t)
    -> c_variables:(C_variables.Map.t -> C_variables.Map.t Or_error.t)
    -> 'b t Or_error.t
  (** [transform config ~format ~postcondition ~c_variables] transforms
      [config] with the given functions. It fails if any of the transformers
      fail. *)
end

module Make (B : Runner.Basic) :
  S
  with type config = B.Src_lang.Constant.t Config.t
   and type fmt = Format.t
   and type program =
              ( B.Src_lang.Element.t
              , B.Src_lang.Program.t )
              Act_sanitiser.Output.Program.t
   and module Redirect := B.Src_lang.Symbol.R_map

val get_filter :
  (module Runner.Basic) -> (module Runner.S with type cfg = Sexp.t Config.t)
(** [get_filter Runner] is [Runner.Litmusify], but with the input type
    altered slightly so that the constants inside any litmus postconditions
    are expected to be S-expressions, and unmarshalled into the appropriate
    language at run-time. *)
