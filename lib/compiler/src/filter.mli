(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Compiler filters.

    These functors and functions lift compilers into the
    {{!Utils.Filter} filter} system, so that they can be composed with other
    similar passes. *)

(** Type of compiler filters. *)
module type S =
  Plumbing.Filter_types.S with type aux_i = Mode.t and type aux_o = unit

(** Lifts an [S] to a filter. *)
module Make (S : Instance_types.S) : S
