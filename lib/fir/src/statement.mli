(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** FIR: top-level statements.

    FIR groups statements into 'primitive' statements, which inhabit the type
    [Prim_statement.t], and composite ones, which inhabit this module's [t]
    type.

    The definition of this signature is somewhat complicated, as it involves
    mutually recursive modules. The parts of this module's signature that
    partake in the recursion aren't defined here, but rather in
    [Statement_types.S_statement]. *)

(** A statement.

    We treat some things that are technically expressions in C as statements,
    for simplicity. *)
type 'meta t [@@deriving sexp, compare, equal]

(** {1 Reducing statements} *)

val reduce_step :
     'meta t
  -> prim:(('meta, Prim_statement.t) With_meta.t -> 'result)
  -> if_stm:(('meta, 'meta t) If.t -> 'result)
  -> flow:(('meta, 'meta t) Flow_block.t -> 'result)
  -> 'result
(** [reduce_step stm ~prim ~if_stm ~flow] applies the appropriate function of
    those given to [stm]. It does _not_ recursively reduce statements inside
    blocks. *)

val reduce :
     'meta t
  -> prim:(('meta, Prim_statement.t) With_meta.t -> 'result)
  -> if_stm:(('meta, 'result) If.t -> 'result)
  -> flow:(('meta, 'result) Flow_block.t -> 'result)
  -> 'result
(** [reduce stm ~prim ~if_stm ~flow] applies the appropriate function of
    those given to [stm]. Unlike {!reduce_step}, it _does_ recursively reduce
    statements inside blocks. *)

val own_metadata : 'meta t -> 'meta Option.t
(** [own_meta stm] gets any metadata directly held by [stm]. It doesn't
    recurse into other nodes (such as blocks); since not all statement types
    directly hold metadata, it returns an option. *)

(** {1 Specialised forms of statement component types}

    These specialised forms contain basic derived operations, but no more.
    Use {!Statement_traverse} for traversal, and the original modules for
    everything else. *)

module If : sig
  (** Specialised if statement type. *)
  type nonrec 'meta t = ('meta, 'meta t) If.t [@@deriving sexp]
end

module Flow_block : sig
  (** Specialised flow block type. *)
  type nonrec 'meta t = ('meta, 'meta t) Flow_block.t [@@deriving sexp]
end

(** {1 Accessors} *)

val if_stm : ('i, 'm If.t, 'm t, [< Accessor.variant]) Accessor.t
(** [if_stm] focuses on if statements. *)

val flow :
  ('i, 'm Flow_block.t, 'm t, [< Accessor.variant]) Accessor.t
(** [flow] focuses on flow blocks. *)

val prim :
  ( 'i
  , ('m, Prim_statement.t) With_meta.t
  , 'm t
  , [< Accessor.variant] )
  Accessor.t
(** [prim] focuses on primitive statements. *)

val prim' :
  ('a, Prim_statement.t, unit t, [< Accessor.variant]) Accessor.t
(** [prim'] is a variant of [prim] that presumes there is no metadata. *)

(** {1 Predicates over statements} *)

val is_prim_and : 'meta t -> f:(Prim_statement.t -> bool) -> bool
(** [is_prim_and x ~f] lifts the predicate [f] over primitive statements to
    one over statements, being [false] over any non-primitive statement
    (including those that contain primitive statements for which [f] is
    true). *)

val is_if_statement : 'meta t -> bool
(** [is_if_statement stm] is true provided that [stm] is an if statement. *)

val has_if_statements : 'meta t -> bool
(** [has_if_statements stm] is true provided that [stm] is an if statement,
    or a composite statement for which [has_if_statements] is true for at
    least one sub-statement. *)

val has_blocks_with_metadata : 'meta t -> predicate:('meta -> bool) -> bool
(** [has_blocks_with_metadata stm ~predicate] is true provided that [stm] has
    at least one block for which [predicate] is true on that block's
    metadata.

    This is useful for tracking things like the existence of dead-code. *)
