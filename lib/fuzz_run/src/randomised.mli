(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** A fuzz runner that applies a random battery of fuzzer actions to a test. *)

open Base
open Import

val run :
     ?seed:int
  -> Fuzz.Subject.Test.t
  -> config:Config.t
  -> Fuzz.Output.t Fuzz.State.Monad.t
(** [run ?seed test ~config] mutates [test] using a random number generator
    seeded by [seed], and the fuzzer config given by [config]. It returns the
    output of the fuzzing operation (including the final test, final variable
    map, and trace of actions applied) inside the main fuzzer state monad. *)
