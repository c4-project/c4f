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

(** [Compiler] contains the high-level interface for specifying and invoking
    compilers. *)

open Core_kernel
open Instance_types

(** A compiler that always passes its tests, but fails with [E.error] on
    runtime.

    Generally used when building a compiler chain fails, but the error is
    one that can be sidestepped over if the compiler never gets run. *)
module Fail (E : sig
  val error : Error.t
end) : S

(** [Property] contains a mini-language for querying compiler specs,
    suitable for use in [Blang]. *)
module Property : sig
  (** [t] is the opaque type of property queries. *)
  type t [@@deriving sexp]

  val id : Act_common.Id.Property.t -> t
  (** [id] constructs a query over a compiler's ID. *)

  val eval : Spec.With_id.t -> t -> bool
  (** [eval cspec property] evaluates [property] over [cspec]. *)

  val eval_b : Spec.With_id.t -> t Blang.t -> bool
  (** [eval_b cspec expr] evaluates a [Blang] expression [expr] over
      [cspec]. *)

  include Act_common.Property.S with type t := t
end

(** [Make] produces a runnable compiler satisfying [S] from a
    [Basic_with_run_info]. *)
module Make (B : Basic_with_run_info) : S

(** {2 Filters}

    These functors and functions lift compilers into the
    {{!Utils.Filter} filter} system, so that they can be composed with other
    similar passes. *)

(** Lifts a [S] to a filter. *)
module S_to_filter (S : S) :
  Plumbing.Filter_types.S with type aux_i = unit and type aux_o = unit

(** Shorthand for [Make_filter (S_to_filter (B))]. *)
module Make_filter (B : Basic_with_run_info) :
  Plumbing.Filter_types.S with type aux_i = unit and type aux_o = unit

(** Abstract type of auxiliary input wrappers used for compiler chains. *)
module Chain_input : sig
  type next_mode = [`Preview | `No_compile | `Compile]

  type 'a t

  val file_type : 'a t -> Act_common.File_type.t

  val next : 'a t -> next_mode -> 'a

  val make :
    file_type:Act_common.File_type.t -> next:(next_mode -> 'a) -> 'a t
end

module Chain_with_compiler
    (Comp : Plumbing.Filter_types.S
            with type aux_i = unit
             and type aux_o = unit)
    (Onto : Plumbing.Filter_types.S) :
  Plumbing.Filter_types.S
  with type aux_i = Onto.aux_i Chain_input.t
   and type aux_o = Onto.aux_o
