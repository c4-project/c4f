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

  val run_and_dump_vars :
       Act_fuzz.Subject.Test.t Act_fuzz.State.Monad.t
    -> predicates:(Act_fuzz.Var.Record.t -> bool) list
    -> initial_state:Act_fuzz.State.t
    -> unit
  (** [run_and_dump_vars action ~predicates ~initial_state] runs [action] on
      [initial_state], finds all variables matching [predicates], and dumps
      them to stdout. *)

  val run_and_dump_globals :
       Act_fuzz.Subject.Test.t Act_fuzz.State.Monad.t
    -> initial_state:Act_fuzz.State.t
    -> unit
  (** [run_and_dump_globals action ~initial_state] runs [action] on
      [initial_state], finds all global variables, and dumps them to stdout. *)

  val run_and_dump_kvs :
       Act_fuzz.Subject.Test.t Act_fuzz.State.Monad.t
    -> initial_state:Act_fuzz.State.t
    -> unit
  (** [run_and_dump_kvs action ~initial_state] runs [action] on
      [initial_state], finds all variables with known values, and dumps them
      to stdout. *)

  val run_and_dump_deps :
       Act_fuzz.Subject.Test.t Act_fuzz.State.Monad.t
    -> initial_state:Act_fuzz.State.t
    -> unit
  (** [run_and_dump_deps action ~initial_state] runs [action] on
      [initial_state], finds all variables with dependencies, and dumps them
      to stdout. *)
end
