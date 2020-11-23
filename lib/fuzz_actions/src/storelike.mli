(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Common plumbing for 'store-like statement' actions.

    This module contains glue for making actions over statements that behave
    partly, or fully, like stores; including {!Store_actions} and
    {!Rmw_actions}. *)

open Import

(** Make makes an action for generating inserting a storelike statement. *)
module Make (B : Storelike_types.Basic) :
  Act_fuzz.Action_types.S
    with type Payload.t = B.t Act_fuzz.Payload_impl.Pathed.t

(** {1 Helpers for writing storelikes} *)

(** [lift_prims ps ~meta] applies [meta] to every prim in [ps] while lifting
    them to statements.  This is useful for defining the [to_stms] of a
    storelike that doesn't introduce control flow. *)
val lift_prims : Fir.Prim_statement.t list -> meta:Fuzz.Metadata.t -> Fuzz.Subject.Statement.t list
