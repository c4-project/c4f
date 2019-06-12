(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** De-litmusification of memalloy-style C litmus tests *)

open Base

val run : Act_c.Mini_litmus.Ast.Validated.t -> Output.t Or_error.t
(** [run litmus] runs de-litmusification on [litmus]. *)
