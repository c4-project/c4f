(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Tests and test data for the fuzzer subject representation. *)

(** {1 Test data}

    An example Litmus test, partitioned into lazily evaluated subcomponents. *)
module Test_data : sig
  val body_stms : C4f_fuzz.Subject.Statement.t list Lazy.t
  (** [body_stms] evaluates to the body statements of the example test. *)

  (** For an initial state, use {!State.Test_data}. *)

  val test : C4f_fuzz.Subject.Test.t Lazy.t
  (** [test] evaluates to a sample test subject. *)

  (** {2 Paths that select parts of the example test} *)

  module Path : sig
    (** {3 Path stubs} *)

    val thread_0_stms : C4f_fuzz.Path.Stms.t -> C4f_fuzz.Path.t Lazy.t
    (** [thread_0_stms rest] constructs a path that visits the statements of
        test thread 0. *)

    val known_true_if : C4f_fuzz.Path.If.t -> C4f_fuzz.Path.t Lazy.t
    (** [known_true_if rest] constructs a path that visits an if statement
        with a known-true conditional. *)

    val known_false_if : C4f_fuzz.Path.If.t -> C4f_fuzz.Path.t Lazy.t
    (** [known_false_if rest] constructs a path that visits an if statement
        with a known-false conditional. *)

    val dead_else : C4f_fuzz.Path.Stms.t -> C4f_fuzz.Path.t Lazy.t
    (** [dead_else rest] constructs a path that visits a dead 'else' leg of
        an if statement. *)

    (** {3 Statement paths} *)

    val in_stm : C4f_fuzz.Path.t Lazy.t
    (** [in_stm] is a full path that points to an arbitrary primitive
        statement. *)

    val in_stm_flagged : C4f_fuzz.Path.With_meta.t Lazy.t
    (** [in_stm_flagged] is a flagged version of {!in_stm}. *)

    (** {3 Insertion paths} *)

    val insert_once_loop_end : C4f_fuzz.Path.With_meta.t Lazy.t
    (** [insert_once_loop_end] is a full path that points to the end of a
        one-execution loop for insertion purposes. *)

    val insert_multi_loop_end : C4f_fuzz.Path.With_meta.t Lazy.t
    (** [insert_multi_loop_end] is a full path that points to the end of a
        multi-execution loop for insertion purposes. *)

    val insert_dead : C4f_fuzz.Path.With_meta.t Lazy.t
    (** [insert_dead] is a full path that points to somewhere in dead code
        that can take statement insertions, but is not in a loop. *)

    val insert_dead_loop : C4f_fuzz.Path.With_meta.t Lazy.t
    (** [insert_dead_loop] is a full path that points to somewhere in dead
        code that can take statement insertions, and is in a loop. *)

    val insert_live : C4f_fuzz.Path.With_meta.t Lazy.t
    (** [insert_live] is a full path that points to somewhere in live code
        that can take statement insertions. *)

    val insert_start : C4f_fuzz.Path.With_meta.t Lazy.t
    (** [insert_start] is a full path that points to the start of a block of
        live code, for statement insertions. *)

    val insert_end : C4f_fuzz.Path.With_meta.t Lazy.t
    (** [insert_end] is a full path that points to the end of a block of live
        code, for statement insertions. *)

    (** {3 Surround paths} *)

    val surround_atomic : C4f_fuzz.Path.With_meta.t Lazy.t
    (** [surround_atomic] is a full path that points to a statement range
        containing atomics, ready for surrounding. *)

    val surround_dead : C4f_fuzz.Path.With_meta.t Lazy.t
    (** [surround_dead] is a full path that points to a statement range
        inside dead-code, ready for surrounding. *)

    val surround_txsafe : C4f_fuzz.Path.With_meta.t Lazy.t
    (** [surround_txsafe] is a full path that points to a statement range
        containing only transaction-safe statements (if any), ready for
        surrounding. *)

    val surround_label_direct : C4f_fuzz.Path.With_meta.t Lazy.t
    (** [surround_label_direct] is a full path that points to a statement
        range directly containing labels. *)

    val surround_label_indirect : C4f_fuzz.Path.With_meta.t Lazy.t
    (** [surround_label_indirect] is a full path that points to a statement
        range indirectly containing labels. *)
  end
end
