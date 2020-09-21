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
module Make (B : sig
  val name : Act_common.Id.t
  (** [name] is the name of the action. *)

  val readme_preamble : string list
  (** [readme_preamble] is the part of the action readme specific to this
      form of the storelike action. *)

  val dst_type : Act_fir.Type.Basic.t
  (** [dst_type] is the value type of the destination. *)

  val path_filter : Act_fuzz.Path_filter.t
  (** [path_filter] is the filter to apply on statement insertion paths
      before considering them for the atomic store. *)

  val extra_dst_restrictions : Dst_restriction.t list
  (** [extra_dst_restrictions] is a list of additional restrictions to place
      on the destination variables (for example, 'must not have
      dependencies'). *)

  module Flags : Storelike_types.Flags

  include Storelike_types.Basic
end) :
  Act_fuzz.Action_types.S
    with type Payload.t = B.t Act_fuzz.Payload_impl.Pathed.t
