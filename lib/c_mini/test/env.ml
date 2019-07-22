(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Act_c_mini
open Env
module Ac = Act_common
module Tx = Travesty_base_exts

let test_env : Type.t Map.M(Ac.C_id).t Lazy.t =
  lazy
    (Map.of_alist_exn
       (module Ac.C_id)
       Type.
         [ (Ac.C_id.of_string "foo", int ())
         ; (Ac.C_id.of_string "bar", int ~pointer:true ~atomic:true ())
         ; (Ac.C_id.of_string "barbaz", bool ())
         ; (Ac.C_id.of_string "foobaz", bool ~pointer:true ~atomic:true ())
         ; (Ac.C_id.of_string "x", int ~atomic:true ())
         ; (Ac.C_id.of_string "y", int ~atomic:true ())
         ; (Ac.C_id.of_string "z", bool ~atomic:true ())
         ; (Ac.C_id.of_string "blep", int ~pointer:true ()) ])

let lift_to_lazy_mod (e : Type.t Ac.C_id.Map.t Lazy.t) :
    (module Env_types.S) Lazy.t =
  Lazy.(
    e
    >>| fun env ->
    ( module Make (struct
      let env = env
    end) : Env_types.S ))

let test_env_mod : (module Env_types.S) Lazy.t = lift_to_lazy_mod test_env

let test_env_atomic_ptrs_only : Type.t Ac.C_id.Map.t Lazy.t =
  Lazy.(
    test_env
    >>| Ac.C_id.Map.filter
          ~f:
            Type.(
              Tx.Fn.(is_pointer &&& basic_type_is ~basic:Basic.(int ~atomic:true ()))))

let test_env_atomic_ptrs_only_mod : (module Env_types.S) Lazy.t =
  lift_to_lazy_mod test_env_atomic_ptrs_only

let empty_env_mod = lift_to_lazy_mod (lazy Ac.C_id.Map.empty)
