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

  val gen :
       src:Act_fir.Env.t
    -> dst:Act_fir.Env.t
    -> vars:Var.Map.t
    -> tid:int
    -> t Base_quickcheck.Generator.t
  (** [gen ~src ~dst ~vars ~tid] returns a quickcheck instance for atomic
      stores given source and destination variable environments [src] and
      [dst], a variable map [var] for fresh variable generation, and the
      target thread ID [tid]. *)

  val new_locals : t -> Act_fir.Initialiser.t Act_common.C_named.Alist.t
  (** [new_locals s] gets a list of any new local variables created for this
      storelike, which should be registered and added to both the thread's
      decls and the state. *)

  val dst_ids : t -> Act_common.C_id.t list
  (** [dst_exprs s] gets a list of any (unscoped) destination C identifiers
      used in this storelike. *)

  val src_exprs : t -> Act_fir.Expression.t list
  (** [src_exprs s] gets a list of any source expressions used in this
      storelike. *)

  val to_stms : t -> Act_fir.Prim_statement.t list
  (** [to_stms s] lifts a storelike into a list of primitives. *)
end
