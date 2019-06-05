(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** High-level interface for specifying and invoking compilers. *)

open Core_kernel

(** A compiler that always passes its tests, but fails with [E.error] on
    runtime.

    Generally used when building a compiler chain fails, but the error is
    one that can be sidestepped over if the compiler never gets run. *)
module Fail (E : sig
  val error : Error.t
end) : Instance_types.S

(** [Make] produces a runnable compiler satisfying [S] from a
    [Basic_with_run_info]. *)
module Make (B : Instance_types.Basic_with_run_info) : Instance_types.S

(** {2 Filters}

    These functors and functions lift compilers into the
    {{!Utils.Filter} filter} system, so that they can be composed with other
    similar passes. *)

(** Lifts a [S] to a filter. *)
module S_to_filter (S : Instance_types.S) :
  Plumbing.Filter_types.S with type aux_i = unit and type aux_o = unit

(** Shorthand for [Make_filter (S_to_filter (B))]. *)
module Make_filter (B : Instance_types.Basic_with_run_info) :
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
