(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** A fuzz runner that applies a random battery of fuzzer actions to a test. *)

open Base
open Act_common

val run :
     ?seed:int
  -> Act_c_mini.Litmus.Ast.Validated.t
  -> o:Output.t
  -> config:Act_config.Fuzz.t
  -> (Act_c_mini.Litmus.Ast.Validated.t * Trace.t) Or_error.t
(** [run ?seed test ~o test] mutates [test] using a random number generator
    seeded by [seed]. Any debugging information is printed to the
    appropriate formatters on [o], and a trace of all run actions is
    returned alongside the new test. *)
