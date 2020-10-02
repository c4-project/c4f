(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

(** Common plumbing for 'store-like statement' actions.

    This module contains glue for making actions over statements that behave
    partly, or fully, like stores; including {!Store_actions} and
    {!Rmw_actions}. *)

(** {1 Helpers for constructing actions} *)

(** {2 Destination restrictions} *)

module Dst_restriction : sig
  (** Type of destination restrictions. *)
  type t = Act_fuzz.Var.Record.t -> bool

  val forbid_dependencies : t
  (** [forbid_dependencies] is a destination restriction that forbids
      destinations that the fuzzer believes have dependencies, either through
      explicit marking or through benefit of the doubt. *)
end

(** {1 Functors} *)

(** Make makes an action for generating inserting a storelike statement. *)
module Make (B : Storelike_types.Basic) :
  Act_fuzz.Action_types.S
    with type Payload.t = B.t Act_fuzz.Payload_impl.Pathed.t
