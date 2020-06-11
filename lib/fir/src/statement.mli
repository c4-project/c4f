(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
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

module I := If
module W := While

(** A statement.

    We treat some things that are technically expressions in C as statements,
    for simplicity. *)
type 'meta t [@@deriving sexp, compare, equal]

module If : Statement_types.S_common with type 'meta t = ('meta, 'meta t) I.t

module While :
  Statement_types.S_common with type 'meta t = ('meta, 'meta t) W.t

include
  Statement_types.S_statement
    with type 'meta t := 'meta t
     and type 'meta if_stm := 'meta If.t
     and type 'meta while_loop := 'meta While.t

(** {1 Reducing statements} *)

val reduce_step :
     'meta t
  -> prim:('meta * Prim_statement.t -> 'result)
  -> if_stm:(('meta, 'meta t) I.t -> 'result)
  -> while_loop:(('meta, 'meta t) W.t -> 'result)
  -> 'result
(** [reduce_step stm ~prim ~if_stm ~while_loop] applies the appropriate
    function of those given to [stm]. It does _not_ recursively reduce
    statements inside blocks. *)

val reduce :
     'meta t
  -> prim:('meta * Prim_statement.t -> 'result)
  -> if_stm:(('meta, 'result) I.t -> 'result)
  -> while_loop:(('meta, 'result) W.t -> 'result)
  -> 'result
(** [reduce stm ~prim ~if_stm ~while_loop] applies the appropriate function
    of those given to [stm]. Unlike {!reduce_step}, it _does_ recursively
    reduce statements inside blocks. *)

(** {1 Constructors} *)

val if_stm : 'meta If.t -> 'meta t
(** [if_stm ifs] lifts an if statement [ifs] to a statement. *)

val while_loop : 'meta While.t -> 'meta t
(** [while_loop loop] lifts a while or do-while loop [loop] to a statement. *)

val prim : 'meta -> Prim_statement.t -> 'meta t
(** [prim meta p] lifts a primitive statement [p] to a statement, with
    metadata [meta]. *)

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

val has_while_loops : 'meta t -> bool
(** [has_while_loops stm] is true provided that [stm] is a while (or
    do-while) loop, or a composite statement for which [has_while_loops] is
    true for at least one sub-statement. *)

val has_blocks_with_metadata : 'meta t -> predicate:('meta -> bool) -> bool
(** [has_blocks_with_metadata stm ~predicate] is true provided that [stm] has
    at least one block for which [predicate] is true on that block's
    metadata.

    This is useful for tracking things like the existence of dead-code. *)
