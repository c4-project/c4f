(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Tests and test data for the fuzzer subject representation. *)

(** An example Litmus test, partitioned into lazily evaluated subcomponents. *)
module Example : sig
  val state : Act_fuzz.State.t Lazy.t
  (** [state] evaluates to the example test's recommended initial state. *)

  val test : Act_fuzz.Metadata.t Act_fuzz.Subject.Test.t Lazy.t
  (** [test] evaluates to a sample test subject. *)
end
