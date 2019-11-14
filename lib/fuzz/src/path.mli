(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Path-based traversal of mini-C and fuzzer-subject elements.

    This module includes submodules for walking down the various mini-C
    constructs used in the fuzzer. *)

(** {1 Helpers} *)

val tid : Path_shapes.program -> int
(** [tid_of_path prog_path] gets the thread ID of the particular thread that
    [prog_path] walks down. *)

(** {1 Specific types of path} *)

module Statement_list :
  Path_types.S_statement_list with type target = Subject.Statement.t

module If_statement :
  Path_types.S_if_statement
    with type target = Metadata.t Act_c_mini.Statement.If.t

module Statement :
  Path_types.S_statement with type target = Subject.Statement.t

module Thread : Path_types.S_function with type target := Subject.Thread.t

module Test : Path_types.S_program with type target := Subject.Test.t
