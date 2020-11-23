(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

let actions : Fuzz.Action.With_default_weight.t list Lazy.t =
  lazy
    Fuzz.Action.With_default_weight.
      [ (module Atomic_cmpxchg.Insert.Int_succeed) @-> 30
      ; (module Atomic_store.Insert.Int_dead) @-> 20
      ; (module Atomic_store.Insert.Int_normal) @-> 20
      ; (module Atomic_store.Insert.Int_redundant) @-> 15
      ; (module Atomic_store.Transform.Xchgify) @-> 15
      ; (module Atomic_fetch.Insert.Int_dead) @-> 20
      ; (module Atomic_fetch.Insert.Int_redundant) @-> 15
      ; (module Atomic_fetch.Cond_insert.Negated_addend) @-> 15
      ; (module Atomic_fetch.Cond_insert.Boundary) @-> 15
      ; (module Flow_dead.Insert.Early_out) @-> 20
      ; (module Flow_dead.Insert.Early_out_loop_end) @-> 20
      ; (module Flow_dead.Insert.Goto) @-> 20
      ; (module Flow_if.Surround.Tautology) @-> 15
      ; (module Flow_if.Surround.Duplicate) @-> 15
      ; (module Flow_if.Transform.Invert) @-> 10
      ; (module Flow_while.Insert.False) @-> 15
      ; (module Flow_while.Surround.Dead) @-> 10
      ; (module Flow_while.Surround.Do_dead) @-> 10
      ; (module Flow_while.Surround.Do_false) @-> 15
      ; (module Flow_for.Insert.Kv_never) @-> 15
      ; (module Flow_for.Surround.Dead) @-> 10
      ; (module Flow_for.Surround.Kv_once) @-> 10
      ; (module Flow_for.Surround.Simple) @-> 15
      ; (module Mem.Fence) @-> 15
      ; (module Mem.Strengthen) @-> 15
      ; (module Program.Make_empty) @-> 10
      ; (module Program.Label) @-> 15
      ; (module Var.Make) @-> 20
      ; (module Var.Volatile) @-> 25
      ; (module Var_assign.Insert.Int_dead) @-> 20
      ; (module Var_assign.Insert.Int_normal) @-> 20
      ; (module Var_assign.Insert.Int_redundant) @-> 15
        (* This action gets fired if every other action is unavailable, so
           giving it a weight here just makes it occasionally get chosen in
           normal play. *)
      ; (module Fuzz.Action.Nop) @-> 5
        (* These are disabled by default because they induce transactions.
           TODO(@MattWindsor91): gate them in another way. *)
      ; (module Flow_lock.Surround.Atomic) @-> 0
      ; (module Flow_lock.Surround.Sync) @-> 0 ]

let action_map :
    Fuzz.Action.With_default_weight.t Map.M(Act_common.Id).t Lazy.t =
  Lazy.(
    actions
    >>| List.map ~f:(fun a -> (Fuzz.Action.With_default_weight.name a, a))
    >>| Map.of_alist_exn (module Act_common.Id))
