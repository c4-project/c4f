(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

(** Test helpers for storelike action tests *)

module Test_common : sig
  val prepare_fuzzer_state : unit -> unit Act_fuzz.State.Monad.t
  (** [prepare_fuzzer_state] populates the fuzzer state with some test data. *)
end
