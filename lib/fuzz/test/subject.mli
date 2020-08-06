(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Tests and test data for the fuzzer subject representation. *)

(** {1 Test data}

    An example Litmus test, partitioned into lazily evaluated subcomponents. *)
module Test_data : sig
  val body_stms : Act_fuzz.Subject.Statement.t list Lazy.t
  (** [body_stms] evaluates to the body statements of the example test. *)

  val state : Act_fuzz.State.t Lazy.t
  (** [state] evaluates to the example test's recommended initial state. *)

  val test : Act_fuzz.Subject.Test.t Lazy.t
  (** [test] evaluates to a sample test subject. *)

  (** {2 Paths that select parts of the example test} *)

  module Path : sig
    (** {3 Path stubs} *)

    val thread_0_stms : Act_fuzz.Path.Stms.t -> Act_fuzz.Path.Test.t Lazy.t
    (** [thread_0_stms rest] constructs a path that visits the statements of
        test thread 0. *)

    val known_true_if : Act_fuzz.Path.If.t -> Act_fuzz.Path.Test.t Lazy.t
    (** [known_true_if rest] constructs a path that visits an if statement
        with a known-true conditional. *)

    val known_false_if : Act_fuzz.Path.If.t -> Act_fuzz.Path.Test.t Lazy.t
    (** [known_false_if rest] constructs a path that visits an if statement
        with a known-false conditional. *)

    val dead_else : Act_fuzz.Path.Stms.t -> Act_fuzz.Path.Test.t Lazy.t
    (** [dead_else rest] constructs a path that visits a dead 'else' leg of
        an if statement. *)

    (** {3 Statement paths} *)
    val in_stm : Act_fuzz.Path.Test.t Lazy.t
    (** [in_stm] is a full path that points to an arbitrary primitive
        statement. *)

    (** {3 Insertion paths} *)

    val insert_dead : Act_fuzz.Path.Test.t Lazy.t
    (** [insert_dead] is a full path that points to somewhere in dead code
        that can take statement insertions. *)

    val insert_live : Act_fuzz.Path.Test.t Lazy.t
    (** [insert_live] is a full path that points to somewhere in live code
        that can take statement insertions. *)

    val insert_start : Act_fuzz.Path.Test.t Lazy.t
    (** [insert_start] is a full path that points to the start of a block of
        live code, for statement insertions. *)

    val insert_end : Act_fuzz.Path.Test.t Lazy.t
    (** [insert_end] is a full path that points to the end of a block of live
        code, for statement insertions. *)

    (** {3 Surround paths} *)

    val surround_atomic : Act_fuzz.Path.Test.t Lazy.t
    (** [surround_atomic] is a full path that points to a statement range
        containing atomics, ready for surrounding. *)

    val surround_dead : Act_fuzz.Path.Test.t Lazy.t
    (** [surround_dead] is a full path that points to a statement range
        inside dead-code, ready for surrounding. *)

    val surround_txsafe : Act_fuzz.Path.Test.t Lazy.t
    (** [surround_txsafe] is a full path that points to a statement range
        containing only transaction-safe statements (if any), ready for
        surrounding. *)

    val surround_label_direct : Act_fuzz.Path.Test.t Lazy.t
    (** [surround_label_direct] is a full path that points to a statement
        range directly containing labels. *)

    val surround_label_indirect : Act_fuzz.Path.Test.t Lazy.t
    (** [surround_label_indirect] is a full path that points to a statement
        range indirectly containing labels. *)
  end
end
