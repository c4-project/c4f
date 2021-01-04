(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Assignment actions. *)

open Import

(** {1 Actions that generate assignments} *)
module Insert : sig
  (** Shorthand type for assignment insertion actions. *)
  module type S =
    Fuzz.Action_types.S
      with type Payload.t = Fir.Assign.t Fuzz.Payload_impl.Pathed.t

  (** [Int_normal] is a fuzzer action that generates a random int assignment. *)
  module Int_normal : S

  (** [Int_dead] is an insertion action that only targets dead-code, and, in
      turn, requires and adds fewer constraints on the destination. *)
  module Int_dead : S

  (** [Int_redundant] is an insertion action that only stores a destination's
      known value back to itself. *)
  module Int_redundant : S

  (** {2 Recommendation helpers} *)

  val int_action_names : Common.Id.t list Lazy.t
  (** [int_action_names] evaluates to a list of all insert actions that are
      available on integers. *)
end
