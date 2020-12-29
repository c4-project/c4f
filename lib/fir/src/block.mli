(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

(** Generic blocks with metadata. *)

(** {1 Type, constructor, and accessors} *)

(** Opaque type of blocks, parametrised over metadata and statement types. *)
type ('meta, 'stm) t [@@deriving sexp, compare, equal]

(** {2 Constructors} *)

val make : ?statements:'stm list -> metadata:'meta -> unit -> ('meta, 'stm) t
(** [make ~metadata ?statements] makes a block given an optional metadata
    list and statement list (both default to the empty list). *)

val of_statement_list : 'stm list -> (unit, 'stm) t
(** [of_statement_list statements] creates a block with no metadata and with
    statements [statements]. *)

(** {2 Accessors} *)

val metadata : ('i, 'meta, ('meta, 'stm) t, [< field]) Accessor.Simple.t
(** [metadata] accesses block metadata. *)

val statements :
  ('i, 'stm list, ('meta, 'stm) t, [< field]) Accessor.Simple.t
(** [statements] accesses block statement lists. *)

val each_statement : ('i, 'stm, ('meta, 'stm) t, [< many]) Accessor.Simple.t
(** [each_statement] accesses each statement in a block individually. *)

val is_empty : (_, _) t -> bool
(** [is_empty block] is true if, and only if, [statements block] is the empty
    list. *)

(** {1 Traversability} *)

(** We can traverse directly over the metadata and statement types of a
    block. *)
include
  Travesty.Bi_traversable_types.S2
    with type ('meta, 'stm) t := ('meta, 'stm) t

(** [On_statements] fixes the type of metadata and permits direct traversal
    over the statements of a block. *)
module On_statements (Meta : T) :
  Travesty.Traversable_types.S1 with type 'stm t := (Meta.t, 'stm) t

(** A pseudo-traversal over the statement lists of a block. *)
module On_meta_statement_list (Stm : T1) : sig
  module On (M : Applicative.S) : sig
    val map_m :
         ('meta, 'meta Stm.t) t
      -> f:('meta Stm.t list -> 'meta Stm.t list M.t)
      -> ('meta, 'meta Stm.t) t M.t
  end
end
