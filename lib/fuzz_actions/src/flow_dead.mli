(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Fuzzer actions that concern miscellaneous control flows in dead code. *)

open Import

(** Actions that insert dead-control flows. *)
module Insert : sig
  (** A fuzzer action that inserts early-out (break, return, etc) statements
      into dead-code. *)
  module Early_out :
    Fuzz.Action_types.S
      with type Payload.t = Fir.Early_out.t Fuzz.Payload_impl.Pathed.t

  (** A fuzzer action that inserts breaks or continues at the end of loops,
      marking statements below that early-out as dead code *)
  module Early_out_loop_end :
    Fuzz.Action_types.S
      with type Payload.t = Fir.Early_out.t Fuzz.Payload_impl.Pathed.t

  (** A fuzzer action that inserts goto statements to random labels into
      dead-code. *)
  module Goto :
    Fuzz.Action_types.S
      with type Payload.t = Common.C_id.t Fuzz.Payload_impl.Pathed.t
end
