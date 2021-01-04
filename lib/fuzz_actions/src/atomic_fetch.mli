(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Actions that generate fetch statements. *)

open Import

(** {1 Insertion} *)

module Insert : sig
  (** Shorthand type for fetch insertion actions. *)
  module type S =
    Fuzz.Action_types.S
      with type Payload.t =
            Fir.Expression.t Fir.Atomic_fetch.t Fuzz.Payload_impl.Pathed.t

  (* TODO(@MattWindsor91): add Int. *)

  (** [Int_dead] is an insert action that only targets dead-code, and, in
      turn, requires and adds fewer constraints on the destination. *)
  module Int_dead : S

  (** [Int_redundant] is an insert action that only fetches with a bias of 0. *)
  module Int_redundant : S
end

(** {1 Insertion of conditionals based on arbitrary fetches} *)

module Cond_insert : sig
  (** Conditional-insert payloads. *)
  module Payload : sig
    (** Type of conditional-insert payloads. *)
    type t =
      { fetch: Fir.Expression.t Fir.Atomic_fetch.t
      ; comparator: Fir.Op.Binary.Rel.t
      ; target: Fir.Expression.t }
    [@@deriving sexp]
  end

  (** Shorthand for action modules doing conditional inserts. *)
  module type S =
    Fuzz.Action_types.S
      with type Payload.t = Payload.t Fuzz.Payload_impl.Pathed.t

  (** Conditional-insert action where the generated fetch subtracts from its
      target the same value that is compared against in the conditional. *)
  module Negated_addend : S

  (** Conditional-insert action where the generated fetch adds or subtracts 1
      from its target and checks whether the result crossed a boundary with
      regards to 0. *)
  module Boundary : S
end
