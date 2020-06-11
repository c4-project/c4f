(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** A fully parametrised while/do-while loop statement.

    In practice, you'll almost certainly want {!Statement.While}, which is
    this module but specialised to {!Statement}s. *)

open Base

(** {1 Kinds of while loop} *)

module Kind : sig
  (** Enumeration of while loop kinds. *)
  type t = While | Do_while [@@deriving sexp, compare, equal]
end

(** {1 While loops proper} *)

(** Opaque type of while loops. *)
type ('meta, 'stm) t [@@deriving sexp, compare, equal]

(** {2 Constructors} *)

val make :
     cond:Expression.t
  -> body:('meta, 'stm) Block.t
  -> kind:Kind.t
  -> ('meta, 'stm) t
(** [make ~cond ~body ~kind] makes a while loop with the given body,
    conditional, and kind. *)

(** {2 Accessors} *)

val cond : (_, _) t -> Expression.t
(** [cond i] gets [i]'s conditional. *)

val body : ('meta, 'stm) t -> ('meta, 'stm) Block.t
(** [body i] gets [i]'s body. *)

val kind : (_, _) t -> Kind.t
(** [kind i] gets [i]'s kind. *)

(** {2 Traversal helpers} *)

module Base_map (A : Applicative.S) : sig
  val bmap :
       ('m1, 's1) t
    -> cond:(Expression.t -> Expression.t A.t)
    -> body:(('m1, 's1) Block.t -> ('m2, 's2) Block.t A.t)
    -> kind:(Kind.t -> Kind.t A.t)
    -> ('m2, 's2) t A.t
  (** [bmap i ~cond ~t_branch ~f_branch] is a basic applicative map over
      while loops. *)
end

val map :
     ('m1, 's1) t
  -> cond:(Expression.t -> Expression.t)
  -> body:(('m1, 's1) Block.t -> ('m2, 's2) Block.t)
  -> kind:(Kind.t -> Kind.t)
  -> ('m2, 's2) t
(** [map i ~cond ~t_branch ~f_branch] is a basic map over while loops. *)
