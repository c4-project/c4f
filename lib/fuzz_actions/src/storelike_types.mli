(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Module signatures for {!Storelike}. *)

open Import

(** Type of modules that set various flags characterising a storelike
    action's behaviour. *)
module type Flags = sig
  val erase_known_values : bool
  (** [erase_known_values] is a flag that, when true, causes the action to
      erase the known value when finished. *)

  val execute_multi_safe : [`Always | `Never | `If_no_cycles]
  (** [execute_multi_safe] is a 3-way flag that governs whether the storelike
      action is safe to execute multiple times. *)
end

(** Type of modules that describe a storelike statement and how to generate
    and manipulate it. *)
module type Basic = sig
  val readme_preamble : string list
  (** [readme_preamble] is the part of the action readme specific to this
      form of the storelike action. *)

  val dst_type : Fir.Type.Basic.t
  (** [dst_type] is the value type of the destination. *)

  val path_filter : Fuzz.Path_filter.t
  (** [path_filter] is the filter to apply on statement insertion paths
      before considering them for the atomic store. *)

  val extra_dst_restrictions : (Fuzz.Var.Record.t -> bool) list
  (** [extra_dst_restrictions] is a list of additional restrictions to place
      on the destination variables (for example, 'must not have
      dependencies'). *)

  (** Flags. *)
  module Flags : Flags

  (** Type of storelike statements. *)
  type t [@@deriving sexp]

  val gen :
       src:Fir.Env.t
    -> dst:Fir.Env.t
    -> vars:Act_fuzz.Var.Map.t
    -> tid:int
    -> t Base_quickcheck.Generator.t
  (** [gen ~src ~dst ~vars ~tid] returns a quickcheck instance for atomic
      stores given source and destination variable environments [src] and
      [dst], a variable map [vars] for fresh variable generation, and the
      target thread ID [tid]. *)

  val new_local_cap : int
  (** [new_local_cap] is the maximum number of new locals that will be added
      for this storelike; the action will refuse to fire if adding this
      amount exceeds the variable cap. *)

  val new_locals : t -> Fir.Initialiser.t Act_common.C_named.Alist.t
  (** [new_locals s] gets a list of any new local variables created for this
      storelike, which should be registered and added to both the thread's
      decls and the state. *)

  val dst_ids : t -> Act_common.C_id.t list
  (** [dst_exprs s] gets a list of any (unscoped) destination C identifiers
      used in this storelike. *)

  val src_exprs : t -> Fir.Expression.t list
  (** [src_exprs s] gets a list of any source expressions used in this
      storelike. *)

  val to_stms : t -> meta:Fuzz.Metadata.t -> Fuzz.Subject.Statement.t list
  (** [to_stms s ~meta] lifts a storelike into a list of statements, and
      should apply [meta] to each. *)

  include
    Fuzz.Action_types.Basic_meta
      with type pld := t Fuzz.Payload_impl.Pathed.t
end
