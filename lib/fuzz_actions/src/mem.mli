(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Actions relating to memory operations and changes to memory order. *)

(** {1 Fence insertion} *)

(** {2 Action} *)

(** [Fence] is an action that inserts random memory fences. *)
module Fence :
  C4f_fuzz.Action_types.S
    with type Payload.t =
      C4f_fir.Atomic_fence.t C4f_fuzz.Payload_impl.Pathed.t

(** {1 Memory order strengthening} *)

(** {2 Payload} *)

module Strengthen_payload : sig
  (** Opaque type of MO-strengthening payloads. *)
  type t [@@deriving sexp]

  val make :
       path:C4f_fuzz.Path.With_meta.t
    -> mo:C4f_fir.Mem_order.t
    -> can_weaken:bool
    -> t
  (** [make ~path ~mo ~can_weaken] constructs a strengthening payload with
      path [path], memory order [mo], and whether or not weakening is
      allowed. *)
end

(** {2 Action} *)

(** [Strengthen] is an action that tries to strengthen memory orders. *)
module Strengthen :
  C4f_fuzz.Action_types.S with type Payload.t = Strengthen_payload.t
