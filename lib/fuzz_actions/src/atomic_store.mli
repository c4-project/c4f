(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Actions that generate store instructions. *)

(** Shorthand type for store insertion actions. *)
module type S_insert =
  Act_fuzz.Action_types.S
    with type Payload.t =
          Act_fir.Atomic_store.t Act_fuzz.Payload_impl.Insertion.t

(** [Insert_int_normal] is a fuzzer action that generates a random atomic-int
    store instruction. *)
module Insert_int_normal : S_insert

(** [Insert_int_dead] is an insertion action that only targets dead-code,
    and, in turn, requires and adds fewer constraints on the destination. *)
module Insert_int_dead : S_insert

(** [Insert_int_redundant] is an insertion action that only stores a
    destination's known value back to itself. *)
module Insert_int_redundant : S_insert

(** [Xchgify] converts store actions to exchange actions. *)
module Xchgify :
  Act_fuzz.Action_types.S with type Payload.t = Act_fuzz.Path.Test.t
