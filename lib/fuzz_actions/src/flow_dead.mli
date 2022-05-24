(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Fuzzer actions that concern miscellaneous control flows in dead code. *)

open Import

(** Actions that insert dead-control flows. *)
module Insert : sig
  module Early_out_payload : sig
    (** Type of early-out payloads. *)
    type t =
      { if_cond: Fir.Expression.t option
            (** Optional condition to use to wrap the early-out. *)
      ; kind: Fir.Early_out.t  (** Kind of early-out to insert. *) }
    [@@deriving sexp]

    (** Shorthand for actions over the early-out payload. *)
    module type S_action =
      Fuzz.Action_types.S with type Payload.t = t Fuzz.Payload_impl.Pathed.t
  end

  (** A fuzzer action that inserts early-out (break, return, etc) statements
      into dead-code; these never have an if-condition. *)
  module Early_out : Early_out_payload.S_action

  (** A fuzzer action that inserts breaks or continues at the end of loops,
      marking statements below that early-out as dead code, and potentially
      surrunding the break/continue in an if-true. *)
  module Early_out_loop_end : Early_out_payload.S_action

  (** A fuzzer action that inserts goto statements to random labels into
      dead-code. *)
  module Goto :
    Fuzz.Action_types.S
      with type Payload.t = Common.C_id.t Fuzz.Payload_impl.Pathed.t
end
