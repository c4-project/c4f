(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Actions that generate store instructions. *)

(** Shorthand type for store actions. *)
module type S =
  Action_types.S
    with type Payload.t = Act_fir.Atomic_store.t Payload.Insertion.t

(** [Int] is a fuzzer action that generates a random atomic-int store
    instruction. *)
module Int : S

(** [Int_dead] is a variant of [Int] that only targets dead-code, and, in
    turn, requires and adds fewer constraints on the destination. *)
module Int_dead : S

(** [Int_redundant] is a variant of [Int] that only stores a destination's
    known value back to itself. *)
module Int_redundant : S

(** [Xchgify] converts store actions to exchange actions. *)
module Xchgify : Action_types.S with type Payload.t = Path.Test.t
