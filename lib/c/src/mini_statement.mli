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
  (Mini_intf.S_statement
    with type address := Mini_address.t
     and type assign := Mini_assign.t
     and type atomic_cmpxchg := Mini_atomic_cmpxchg.t
     and type atomic_store := Mini_atomic_store.t
     and type identifier := Act_c_lang.Ast_basic.Identifier.t
     and type if_stm := If.t
     and type lvalue := Mini_lvalue.t)

and If :
  (Mini_intf.S_if_statement
    with type expr := Mini_expression.t
     and type stm := Main.t
     and type address := Mini_address.t
     and type identifier := Act_c_lang.Ast_basic.Identifier.t
     and type lvalue := Mini_lvalue.t)

include module type of Main with type t = Main.t
