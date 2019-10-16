(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Converting an AST into the mini-model.

    This module contains partial functions that try to convert a full C AST
    into the mini-model. They fail if the AST contains pieces of C syntax
    that aren't expressible in the mini-model. *)

open Base

val constant : Act_c_lang.Ast_basic.Constant.t -> Constant.t Or_error.t
(** [constant ast] tries to interpret a C constant AST as a mini-model
    constant. *)

val func : Act_c_lang.Ast.Function_def.t -> Function.t Named.t Or_error.t
(** [func ast] tries to interpret a C function definition AST as a
    mini-model function. *)

val translation_unit :
  Act_c_lang.Ast.Translation_unit.t -> Program.t Or_error.t
(** [translation_unit ast] tries to interpret a C translation unit AST as a
    mini-model program. *)

val litmus_post :
     Act_c_lang.Ast_basic.Constant.t Act_litmus.Postcondition.t
  -> Constant.t Act_litmus.Postcondition.t Or_error.t
(** [litmus_post pc] tries to interpret a Litmus postcondition [pc] over the
    full C AST as one over the mini-model. *)

val litmus : Act_c_lang.Ast.Litmus.t -> Litmus.Test.t Or_error.t
(** [litmus test] tries to interpret a Litmus test over the full C AST as
    one over the mini-model. *)

val litmus_of_raw_ast :
  Act_litmus.Ast.M(Act_c_lang.Ast.Litmus_lang).t -> Litmus.Test.t Or_error.t
(** [litmus_of_raw_ast test] applies [litmus] to the validated form, if
    available, of [test]. *)
