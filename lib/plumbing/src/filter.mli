(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Modules for building 'filters', in the UNIX sense.

    A filter is a process that takes input from one file and returns output
    in another. This module contains signatures and functors for building and
    composing filters over the [Io] abstractions. *)

(* TODO(@MattWindsor91): this module is deprecated and should be removed
   eventually. *)

open Filter_types

(** {2 Making filters} *)

(** Makes a filter from a {{!Basic} Basic}. *)
module Make (B : Basic) :
  S with type aux_i = B.aux_i and type aux_o = B.aux_o
