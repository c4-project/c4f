(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Tests and test utilities for {!Act_fuzz.Action}. *)

(** {1 Test utilities} *)

module Test_utils : sig
  val run_and_dump_test :
       Act_fuzz.Subject.Test.t Act_fuzz.State.Monad.t
    -> initial_state:Act_fuzz.State.t
    -> unit
  (** [run_and_dump_test action ~initial_state] runs the monadic computation
      [action] (representing a fuzzer action) on [initial_state], then dumps
      the resulting C to stdout. *)

  val run_and_dump_global_deps :
       Act_fuzz.Subject.Test.t Act_fuzz.State.Monad.t
    -> initial_state:Act_fuzz.State.t
    -> unit
  (** [run_and_dump_global_deps action ~initial_state] runs the monadic
      computation [action] (representing a fuzzer action) on [initial_state],
      then dumps a list of global variables with registered dependencies to
      stdout. *)
end
