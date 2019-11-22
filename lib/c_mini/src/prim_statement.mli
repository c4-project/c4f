(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Mini-C: primitive statments.

    A primitive statement is any statement that doesn't contain other
    statements recursively.

    As a lot of mini-C analysis involves whether a statement is a control
    flow or not, it makes sense to group these statements into one block.

    Note that we treat some things that are technically expressions in C as
    statements in mini-C. This is for simplicity; it rejects some well-formed
    C programs, but chimes better with how we expect the expressions to be
    used. *)

open Base

type 'meta t [@@deriving sexp, equal]
(** Opaque type of mini-C primitive statements, parametrised by metadata. *)

(** {1 Constructors} *)

val assign : Assign.t -> 'meta t
(** [assign a] lifts an assignment to a statement. *)

val atomic_store : Atomic_store.t -> 'meta t
(** [atomic_store a] lifts an atomic store to a statement. *)

val atomic_cmpxchg : Atomic_cmpxchg.t -> 'meta t
(** [atomic_cmpxchg a] lifts an atomic compare-exchange to a statement. *)

val return : 'meta -> 'meta t
(** [return m] creates a return statement with metadata [m]. *)

val nop : 'meta -> 'meta t
(** [return m] creates a no-op (semicolon) statement with metadata [m]. *)

(** {1 Traversals} *)

val reduce :
     'meta t
  -> assign:((* 'meta *) Assign.t -> 'result)
  -> atomic_store:((* 'meta *) Atomic_store.t -> 'result)
  -> atomic_cmpxchg:((* 'meta *) Atomic_cmpxchg.t -> 'result)
  -> return:('meta -> 'result)
  -> nop:('meta -> 'result)
  -> 'result
(** [reduce x ~assign ~atomic_store ~atomic_cmpxchg ~return ~nop] reduces a
    primitive statement [x] to a particular result type by applying the
    appropriate function. *)

(** Creates a basic monadic map over [M]. *)
module Base_map (M : Monad.S) : sig
  val bmap :
       'm1 t
    -> assign:((* 'm1 *) Assign.t -> (* 'm2 *) Assign.t M.t)
    -> atomic_store:
         ((* 'm1 *) Atomic_store.t -> (* 'm2 *) Atomic_store.t M.t)
    -> atomic_cmpxchg:
         ((* 'm1 *) Atomic_cmpxchg.t -> (* 'm2 *) Atomic_cmpxchg.t M.t)
    -> return:('m1 -> 'm2 M.t)
    -> nop:('m1 -> 'm2 M.t)
    -> 'm2 t M.t
  (** [bmap x ~assign ~atomic_store ~atomic_cmpxchg ~return ~nop] maps the
      appropriate function over [x]. *)
end

module On_meta : Travesty.Traversable_types.S1 with type 'meta t := 'meta t
(** A traversal over the metadata in a primitive statement. *)

(** {2 Traversals with a fixed metadata type}

    To do traversals over parts of a primitive statement that aren't the
    metadata, we must fix the metadata in a functor. *)
module With_meta (Meta : T) : sig
  (** Traverses over the lvalues of a primitive expression. *)
  module On_lvalues :
    Travesty.Traversable_types.S0
      with type t = Meta.t t
       and type Elt.t = Lvalue.t

  (** Traverses over the addresses of a primitive expression. *)
  module On_addresses :
    Travesty.Traversable_types.S0
      with type t = Meta.t t
       and type Elt.t = Address.t

  (** Traverses over the identifiers of a primitive expression. *)
  module On_identifiers :
    Travesty.Traversable_types.S0
      with type t = Meta.t t
       and type Elt.t = Act_common.C_id.t
end
