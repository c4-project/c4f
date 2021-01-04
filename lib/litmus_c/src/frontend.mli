(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Language frontends for C *)

(** Frontend for 'normal' C (C89 at time of writing). *)
module Normal :
  Plumbing.Loadable_types.S with type t = Ast.Translation_unit.t

(** Frontend for C-based litmus tests. *)
module Litmus :
  Plumbing.Loadable_types.S
    with type t =
          ( Ast.Litmus_lang.Constant.t
          , Ast.Litmus_lang.Program.t )
          C4f_litmus.Ast.t

(** Frontend for postconditions; used specifically for parsing auxiliary JSON
    files. *)
module Litmus_post :
  Plumbing.Loadable_types.S
    with type t = Ast_basic.Constant.t C4f_litmus.Postcondition.t

(** Frontend for FIR, using {!Litmus} as an intermediary. *)
module Fir : Plumbing.Loadable_types.S with type t = C4f_fir.Litmus.Test.t
