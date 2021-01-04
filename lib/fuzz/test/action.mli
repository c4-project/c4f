(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Tests and test utilities for {!C4f_fuzz.Action}. *)

(** {1 Test utilities} *)

module Test_utils : sig
  val reify_test :
       C4f_fuzz.Subject.Test.t
    -> C4f_fuzz.Var.Record.t C4f_common.Scoped_map.t
    -> C4f_litmus_c.Ast.Translation_unit.t
  (** [reify_test test vars] reifies [test] into a translation unit, using
      [vars] as its variable map. *)

  val pp_tu : C4f_litmus_c.Ast.Translation_unit.t Fmt.t
  (** [pp_tu] prints a translation unit. *)

  val run_and_dump_test :
       C4f_fuzz.Subject.Test.t C4f_fuzz.State.Monad.t
    -> initial_state:C4f_fuzz.State.t
    -> unit
  (** [run_and_dump_test action ~initial_state] runs the monadic computation
      [action] (representing a fuzzer action) on [initial_state], then dumps
      the resulting C to stdout along with the final variable map. *)
end
