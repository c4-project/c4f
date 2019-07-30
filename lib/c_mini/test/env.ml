(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Src = Act_c_mini
module Ac = Act_common
module Tx = Travesty_base_exts

let test_env : Src.Type.t Map.M(Ac.C_id).t Lazy.t =
  lazy
    (Map.of_alist_exn
       (module Ac.C_id)
       Src.Type.
         [ (Ac.C_id.of_string "foo", int ())
         ; (Ac.C_id.of_string "bar", int ~pointer:true ~atomic:true ())
         ; (Ac.C_id.of_string "barbaz", bool ())
         ; (Ac.C_id.of_string "foobaz", bool ~pointer:true ~atomic:true ())
         ; (Ac.C_id.of_string "x", int ~atomic:true ())
         ; (Ac.C_id.of_string "y", int ~atomic:true ())
         ; (Ac.C_id.of_string "z", bool ~atomic:true ())
         ; (Ac.C_id.of_string "blep", int ~pointer:true ()) ])

let lift_to_lazy_mod (e : Src.Type.t Map.M(Ac.C_id).t Lazy.t) :
    (module Src.Env_types.S) Lazy.t =
  Lazy.(
    e
    >>| fun env ->
    ( module Src.Env.Make (struct
      let env = env
    end) : Src.Env_types.S ))

let test_env_mod : (module Src.Env_types.S) Lazy.t =
  lift_to_lazy_mod test_env

let test_env_atomic_ptrs_only : Src.Type.t Map.M(Ac.C_id).t Lazy.t =
  Lazy.(
    test_env >>| Map.filter ~f:Src.Type.(Tx.Fn.(is_pointer &&& is_atomic)))

let test_env_atomic_ptrs_only_mod : (module Src.Env_types.S) Lazy.t =
  lift_to_lazy_mod test_env_atomic_ptrs_only

let test_env_scalars_only : Src.Type.t Map.M(Ac.C_id).t Lazy.t =
  Lazy.(test_env >>| Map.filter ~f:Src.Type.(Fn.(non is_pointer)))

let test_env_scalars_only_mod : (module Src.Env_types.S) Lazy.t =
  lift_to_lazy_mod test_env_scalars_only

let empty_env_mod = lift_to_lazy_mod (lazy (Map.empty (module Ac.C_id)))

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
         ; (Ac.C_id.of_string "blep", int 99) ])

let det_known_value_mod : (module Src.Env_types.S_with_known_values) Lazy.t
    =
  Lazy.Let_syntax.(
    let%bind (module Env) = test_env_mod in
    let%map known_values = det_known_values in
    ( module Src.Env.Make_with_known_values (struct
      include Env

      let known_values = known_values
    end) : Src.Env_types.S_with_known_values ))
