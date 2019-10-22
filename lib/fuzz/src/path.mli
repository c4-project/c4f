(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Path-based traversal of mini-C elements.

    This module includes functors that create functionality for walking down
    the various mini-C constructs used in the fuzzer. *)

module Statement_list :
  Path_types.S_statement_list
    with type target = Metadata.t Act_c_mini.Statement.t

module If_statement :
  Path_types.S_if_statement
    with type target = Metadata.t Act_c_mini.Statement.If.t

module Statement :
  Path_types.S_statement
    with type target = Metadata.t Act_c_mini.Statement.t

module Function :
  Path_types.S_function with type target := Metadata.t Act_c_mini.Function.t

module Program :
  Path_types.S_program with type target := Metadata.t Act_c_mini.Program.t