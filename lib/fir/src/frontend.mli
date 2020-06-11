(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Frontend for FIR litmus tests.

    This module allows code to read FIR litmus tests in directly, rather
    than reading a full C litmus test then converting manually. It also
    therefore insulates against future changes in the abstract/full C
    representation. *)

include Plumbing.Loadable_types.S with type t = Litmus.Test.t
