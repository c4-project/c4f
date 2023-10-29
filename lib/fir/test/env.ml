(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Src = C4f_fir
module Ac = C4f_common
module Tx = Travesty_base_exts

let test_typing : Src.Type.t Map.M(Ac.C_id).t Lazy.t =
  lazy
    (Map.of_alist_exn
       (module Ac.C_id)
       Src.Type.
         [ (Ac.C_id.of_string "foo", int ())
         ; (Ac.C_id.of_string "bar", int ~is_pointer:true ~is_atomic:true ())
         ; (Ac.C_id.of_string "barbaz", bool ())
         ; ( Ac.C_id.of_string "foobaz"
           , bool ~is_pointer:true ~is_atomic:true () )
         ; (Ac.C_id.of_string "x", int ~is_atomic:true ())
         ; (Ac.C_id.of_string "y", int ~is_atomic:true ())
         ; (Ac.C_id.of_string "z", bool ~is_atomic:true ())
         ; (Ac.C_id.of_string "blep", int ~is_pointer:true ()) ] )

let det_known_values : Src.Constant.t Map.M(Ac.C_id).t Lazy.t =
  lazy
    (Map.of_alist_exn
       (module Ac.C_id)
       Src.Constant.
         [ (Ac.C_id.of_string "foo", int 4)
         ; (Ac.C_id.of_string "bar", int 95)
         ; (Ac.C_id.of_string "barbaz", bool true)
         ; (Ac.C_id.of_string "foobaz", bool true)
         ; (Ac.C_id.of_string "x", int 27)
         ; (Ac.C_id.of_string "y", int 53)
         ; (Ac.C_id.of_string "z", bool false)
         ; (Ac.C_id.of_string "blep", int 99) ] )

let lift_to_full (e : Src.Type.t Map.M(Ac.C_id).t Lazy.t) : Src.Env.t Lazy.t
    =
  Lazy.Let_syntax.(
    let%map e = e and d = det_known_values in
    Src.Env.of_maps e d )

let test_env : Src.Env.t Lazy.t = lift_to_full test_typing

let test_typing_atomic_ptrs_only : Src.Type.t Map.M(Ac.C_id).t Lazy.t =
  Lazy.(
    test_typing >>| Map.filter ~f:Src.Type.(Tx.Fn.(is_pointer &&& is_atomic)) )

let test_env_atomic_ptrs_only : Src.Env.t Lazy.t =
  lift_to_full test_typing_atomic_ptrs_only

let test_typing_scalars_only : Src.Type.t Map.M(Ac.C_id).t Lazy.t =
  Lazy.(test_typing >>| Map.filter ~f:Src.Type.(Fn.(non is_pointer)))

let test_env_scalars_only : Src.Env.t Lazy.t =
  lift_to_full test_typing_scalars_only

let empty_env : Src.Env.t Lazy.t =
  lift_to_full (lazy (Map.empty (module Ac.C_id)))
