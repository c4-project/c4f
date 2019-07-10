(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

type t
(** Opaque type of delitmus context. *)

(** {2 Constructors} *)

val make :
     aux:Aux.t
  -> local_inits:( int
                 , ( Act_common.C_id.t
                   , Act_c_lang.Ast_basic.Constant.t )
                   List.Assoc.t )
                 List.Assoc.t
  -> t

(** {2 Components} *)

val var_map : t -> Var_map.t

val lookup_initial_value :
  t -> id:Act_common.Litmus_id.t -> Act_c_lang.Ast_basic.Constant.t option
