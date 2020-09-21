(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Compare-exchange actions. *)

(** {1 Insertions}

    These {!Storelike} actions produce compare-exchanges. *)
module Insert : sig
  (** Inner payload for insertions. *)
  module Inner_payload : sig
    type t =
      { out_var: Act_common.Litmus_id.t
      ; exp_var: Act_common.Litmus_id.t
      ; exp_val: Act_fir.Constant.t
      ; cmpxchg: Act_fir.Expression.t Act_fir.Atomic_cmpxchg.t }
    [@@deriving compare, sexp]
  end

  (** {2 Integer compare-exchanges} *)

  (** Inserts an atomic int compare-exchange that always succeeds, and a new
      local Boolean variable that receives its result. *)
  module Int_succeed :
    Act_fuzz.Action_types.S
      with type Payload.t = Inner_payload.t Act_fuzz.Payload_impl.Pathed.t
end
