(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Assignment actions. *)

(** {1 Actions that generate assignments} *)
module Insert : sig
  (** Shorthand type for assignment insertion actions. *)
  module type S =
    Act_fuzz.Action_types.S
      with type Payload.t =
            Act_fir.Assign.t Act_fuzz.Payload_impl.Insertion.t

  (** [Int_normal] is a fuzzer action that generates a random int assignment. *)
  module Int_normal : S

  (** [Int_dead] is an insertion action that only targets dead-code, and, in
      turn, requires and adds fewer constraints on the destination. *)
  module Int_dead : S

  (** [Int_redundant] is an insertion action that only stores a destination's
      known value back to itself. *)
  module Int_redundant : S
end
