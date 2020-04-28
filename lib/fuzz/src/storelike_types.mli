(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Module signatures for {!Storelike}. *)

(** Type of modules that set various flags characterising a storelike
    action's behaviour. *)
module type Flags = sig
  val respect_src_dependencies : bool
  (** [respect_src_dependencies] is a flag that, when true, causes the action
      to mark dependencies on source variables when finished. *)

  val erase_known_values : bool
  (** [erase_known_values] is a flag that, when true, causes the action to
      erase the known value when finished. *)
end

(** Type of modules that describe a storelike statement and how to generate
    and manipulate it. *)
module type Basic = sig
  (** Type of storelike statements. *)
  type t [@@deriving sexp]

  (** A functor that produces a quickcheck instance for atomic stores given
      source and destination variable environments. *)
  module Quickcheck
      (Src : Act_c_mini.Env_types.S)
      (Dst : Act_c_mini.Env_types.S) :
    Act_utils.My_quickcheck.S_with_sexp with type t := t

  val dst_ids : t -> Act_common.C_id.t list
  (** [dst_exprs] gets a list of any (unscoped) destination C identifiers
      used in this storelike statement. *)

  val src_exprs : t -> Act_c_mini.Expression.t list
  (** [src_exprs] gets a list of any source expressions used in this
      storelike statement. *)

  val to_stm : t -> Act_c_mini.Prim_statement.t
  (** [to_stm] lifts a storelike into a primitive. *)
end
