(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** FIR: blocks with control flow.

    This module abstracts over anything that contains precisely one block
    with an additional 'header' specifying how control flows into and out of
    it. Examples include:

    - for loops;
    - while loops;
    - locks;
    - transactions. *)

open Base

(** {1 Flow block headers} *)

module For : sig
  (** Opaque type of (constrained) for-loop headers. *)
  type t [@@deriving sexp, compare, equal]

  val make :
       lvalue:Lvalue.t
    -> init_value:Expression.t
    -> cmp_value:Expression.t
    -> cmp_range:[`Inclusive | `Exclusive]
    -> direction:[`Down_to | `To]
    -> t
  (** [make ~lvalue ~init_value ~cmp_value ~cmp_range ~direction] constructs
      an arithmetic for-loop header over [lvalue], which starts at
      [init_value], moves in [direction] towards [cmp_value], and behaves as
      per [cmp_range]. *)

  (* TODO(@MattWindsor91): declarations inside for loops *)

  val lvalue : t -> Lvalue.t
  (** [lvalue hdr] gets [hdr]'s counter lvalue. *)

  val init_value : t -> Expression.t
  (** [init_value hdr] gets [hdr]'s initial value. *)

  val cmp_value : t -> Expression.t
  (** [cmp_value hdr] gets [hdr]'s compare-against value. *)

  val cmp_range : t -> [`Inclusive | `Exclusive]
  (** [cmp_range hdr] gets whether [hdr] should iterate when it reaches
      {!cmp_value}. *)

  val direction : t -> [`Down_to | `To]
  (** [direction hdr] gets [hdr]'s direction. *)
end

(** {2 Kinds of flow} *)

module While : sig
  (** Enumeration of types of while loop. *)
  type t = Do_while  (** A do-while loop. *) | While  (** A while loop. *)

  include Act_utils.Enum_types.Extension_table with type t := t
end

module Lock : sig
  (** Enumeration of types of lock block. *)
  type t =
    | Atomic  (** Atomic (strongly isolated transaction) blocks. *)
    | Synchronized
        (** Synchronised (weakly isolated pseudo-transaction) blocks. *)
  [@@deriving sexp, compare, equal]
end

(** {2 Headers proper} *)

module Header : sig
  (** Enumeration of flow block headers. *)
  type t =
    | For of For.t  (** A for-loop head. *)
    | Lock of Lock.t  (** A lock block. *)
    | While of While.t * Expression.t
        (** A while or do-while loop, alongside its condition. *)
  [@@deriving sexp, compare, equal]

  (** {3 Traversals} *)

  (** Traversal over the expressions inside a header. *)
  module On_expressions :
    Travesty.Traversable_types.S0
      with type t := t
       and type Elt.t := Expression.t
end

(** {1 Flow blocks proper} *)

(** Opaque type of while loops. *)
type ('meta, 'stm) t [@@deriving sexp, compare, equal]

(** {2 Constructors} *)

val make : header:Header.t -> body:('meta, 'stm) Block.t -> ('meta, 'stm) t
(** [make ~header ~body] makes a flow block with the given header and body. *)

(** {3 Convenience constructors} *)

val while_loop :
     cond:Expression.t
  -> body:('meta, 'stm) Block.t
  -> kind:While.t
  -> ('meta, 'stm) t
(** [while ~cond ~body ~kind] makes a while loop with the given body,
    conditional, and kind. *)

val lock_block : body:('meta, 'stm) Block.t -> kind:Lock.t -> ('meta, 'stm) t
(** [lock_block ~body ~kind] makes a lock block with the given body and kind. *)

(** {2 Accessors and queries} *)

val body : ('meta, 'stm) t -> ('meta, 'stm) Block.t
(** [body i] gets [i]'s body. *)

val header : (_, _) t -> Header.t
(** [header i] gets [i]'s header. *)

(** {2 Traversal helpers} *)

module Base_map (A : Applicative.S) : sig
  val bmap :
       ('m1, 's1) t
    -> header:(Header.t -> Header.t A.t)
    -> body:(('m1, 's1) Block.t -> ('m2, 's2) Block.t A.t)
    -> ('m2, 's2) t A.t
  (** [bmap i ~header ~body] is a basic applicative map over flow blocks. *)
end

val map :
     ('m1, 's1) t
  -> header:(Header.t -> Header.t)
  -> body:(('m1, 's1) Block.t -> ('m2, 's2) Block.t)
  -> ('m2, 's2) t
(** [map i ~header ~body] is a basic map over flow blocks. *)
