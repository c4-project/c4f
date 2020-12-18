(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Compare-exchange actions. *)

open Import

(** {1 Insertions}

    These {!Storelike} actions produce compare-exchanges. *)
module Insert : sig
  (** Inner payload for insertions. *)
  module Inner_payload : sig
    type t =
      { out_var: Common.C_id.t
      ; exp_var: Common.C_id.t
      ; exp_val: Fir.Constant.t
      ; cmpxchg: Fir.Expression.t Fir.Atomic_cmpxchg.t }
    [@@deriving compare, sexp]
  end

  module type S =
    Fuzz.Action_types.S
      with type Payload.t = Inner_payload.t Fuzz.Payload_impl.Pathed.t

  (** {2 Integer compare-exchanges} *)

  (** Inserts a strong atomic int compare-exchange that always succeeds, and
      a new local Boolean variable that receives its result. *)
  module Int_succeed : S

  (** Inserts a strong or weak atomic int compare-exchange that always
      succeeds, and a new local Boolean variable that receives its result. *)
  module Int_fail : S

  (** Inserts an arbitrary strong or weak atomic int compare-exchange, and a
      new local Boolean variable that receives its result. *)
  module Int_arbitrary : S
end
