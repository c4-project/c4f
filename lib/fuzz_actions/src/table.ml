(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module F = Act_fuzz
end

let actions : F.Action.With_default_weight.t list Lazy.t =
  lazy
    F.Action.With_default_weight.
      [ make
          ~action:(module Atomic_cmpxchg.Insert.Int_always_succeed)
          ~default_weight:30
      ; make ~action:(module Atomic_store.Insert.Int_dead) ~default_weight:20
      ; make
          ~action:(module Atomic_store.Insert.Int_normal)
          ~default_weight:20
      ; make
          ~action:(module Atomic_store.Insert.Int_redundant)
          ~default_weight:15
      ; make
          ~action:(module Atomic_store.Transform.Xchgify)
          ~default_weight:15
      ; make ~action:(module Atomic_fetch.Insert.Int_dead) ~default_weight:20
      ; make
          ~action:(module Atomic_fetch.Insert.Int_redundant)
          ~default_weight:20
      ; make ~action:(module Flow_dead.Insert.Early_out) ~default_weight:20
      ; make ~action:(module Flow_dead.Insert.Goto) ~default_weight:20
      ; make ~action:(module Flow_if.Surround.Tautology) ~default_weight:15
      ; make ~action:(module Flow_if.Surround.Duplicate) ~default_weight:15
      ; make ~action:(module Flow_if.Transform.Invert) ~default_weight:10
      ; make ~action:(module Flow_loop.Insert.While_false) ~default_weight:15
      ; make ~action:(module Flow_loop.Surround.Do_dead) ~default_weight:10
      ; make ~action:(module Flow_loop.Surround.Do_false) ~default_weight:15
      ; make
          ~action:(module Flow_loop.Surround.While_dead)
          ~default_weight:10
      ; make ~action:(module Mem.Fence) ~default_weight:15
      ; make ~action:(module Mem.Strengthen) ~default_weight:15
      ; make ~action:(module Program.Make_empty) ~default_weight:10
      ; make ~action:(module Program.Label) ~default_weight:15
      ; make ~action:(module Var.Make) ~default_weight:20
      ; make ~action:(module Var.Volatile) ~default_weight:25
        (* These are disabled by default because they induce transactions.
           TODO(@MattWindsor91): gate them in another way. *)
      ; make ~action:(module Flow_lock.Surround.Atomic) ~default_weight:0
      ; make ~action:(module Flow_lock.Surround.Sync) ~default_weight:0 ]

let action_map : F.Action.With_default_weight.t Map.M(Act_common.Id).t Lazy.t
    =
  Lazy.(
    actions
    >>| List.map ~f:(fun a -> (F.Action.With_default_weight.name a, a))
    >>| Map.of_alist_exn (module Act_common.Id))
