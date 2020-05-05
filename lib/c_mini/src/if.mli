(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** A fully parametrised if statement.

    In practice, you'll almost certainly want {!Statement.If}, which is this
    module but specialised to {!Statement}s. *)

open Base

(** Opaque type of if statements. *)
type ('meta, 'stm) t [@@deriving sexp, compare, equal]

(** {1 Constructors} *)

val make :
     cond:Expression.t
  -> t_branch:('meta, 'stm) Block.t
  -> f_branch:('meta, 'stm) Block.t
  -> ('meta, 'stm) t
(** [make ~cond ~t_branch ~f_branch] makes an if statement with the given
    branches and conditional. *)

(** {1 Accessors} *)

val cond : (_, _) t -> Expression.t
(** [cond i] gets [i]'s conditional. *)

val t_branch : ('meta, 'stm) t -> ('meta, 'stm) Block.t
(** [t_branch i] gets [i]'s true branch. *)

val f_branch : ('meta, 'stm) t -> ('meta, 'stm) Block.t
(** [f_branch i] gets [i]'s false branch. *)

(** {1 Traversal helpers} *)

module Base_map (A : Applicative.S) : sig
  val bmap :
       ('m1, 's1) t
    -> cond:(Expression.t -> Expression.t A.t)
    -> t_branch:(('m1, 's1) Block.t -> ('m2, 's2) Block.t A.t)
    -> f_branch:(('m1, 's1) Block.t -> ('m2, 's2) Block.t A.t)
    -> ('m2, 's2) t A.t
  (** [bmap i ~cond ~t_branch ~f_branch] is a basic applicative map over if
      statements. *)
end

val map :
     ('m1, 's1) t
  -> cond:(Expression.t -> Expression.t)
  -> t_branch:(('m1, 's1) Block.t -> ('m2, 's2) Block.t)
  -> f_branch:(('m1, 's1) Block.t -> ('m2, 's2) Block.t)
  -> ('m2, 's2) t
(** [map i ~cond ~t_branch ~f_branch] is a basic map over if statements. *)
