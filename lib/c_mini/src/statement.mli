(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Mini C: top-level statements. *)

(** A statement.

    We treat some things that are technically expressions in C as
    statements, for simplicity. *)
module rec Main :
  (Types.S_statement
    with type address := Address.t
     and type assign := Assign.t
     and type atomic_cmpxchg := Atomic_cmpxchg.t
     and type atomic_store := Atomic_store.t
     and type identifier := Act_c_lang.Ast_basic.Identifier.t
     and type if_stm := If.t
     and type lvalue := Lvalue.t)

and If :
  (Types.S_if_statement
    with type expr := Expression.t
     and type stm := Main.t
     and type address := Address.t
     and type identifier := Act_c_lang.Ast_basic.Identifier.t
     and type lvalue := Lvalue.t)

include module type of Main with type t = Main.t
