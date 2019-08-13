(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** A fuzz runner that applies a random battery of fuzzer actions to a test. *)

open Base

val run :
     ?seed:int
  -> Subject.Test.t
  -> config:Config.t
  -> Trace.t Output.t State.Monad.t
(** [run ?seed test ~config] mutates [test] using a random number generator
    seeded by [seed], and the fuzzer config given by [config]. It returns
    the output of the fuzzing operation (including the final test, a trace
    of all actions applied, and the final variable map) inside the main
    fuzzer state monad. *)
