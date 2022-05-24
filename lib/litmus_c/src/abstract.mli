(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Converting an AST into FIR.

    This module contains partial functions that try to convert a full C AST
    into FIR. They fail if the AST contains pieces of C syntax that aren't
    expressible in FIR. *)

open Base

val func :
     Ast.Function_def.t
  -> unit C4f_fir.Function.t C4f_common.C_named.t Or_error.t
(** [func ast] tries to interpret a C function definition AST as a FIR
    function. *)

val translation_unit :
  Ast.Translation_unit.t -> unit C4f_fir.Program.t Or_error.t
(** [translation_unit ast] tries to interpret a C translation unit AST as a
    FIR program. *)

val litmus_post :
     Ast_basic.Constant.t C4f_litmus.Postcondition.t
  -> C4f_fir.Constant.t C4f_litmus.Postcondition.t Or_error.t
(** [litmus_post pc] tries to interpret a Litmus postcondition [pc] over the
    full C AST as one over FIR. *)

val litmus : Ast.Litmus.t -> C4f_fir.Litmus.Test.t Or_error.t
(** [litmus test] tries to interpret a Litmus test over the full C AST as one
    over FIR. *)

val litmus_of_raw_ast :
  C4f_litmus.Ast.M(Ast.Litmus_lang).t -> C4f_fir.Litmus.Test.t Or_error.t
(** [litmus_of_raw_ast test] applies [litmus] to the validated form, if
    available, of [test]. *)
