(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** A fully parametrised if statement.

    In practice, you'll almost certainly want {!Statement.If}, which is this
    module but specialised to {!Statement}s. *)

open Base
open Import

(** Type of if statements. *)
type ('meta, 'stm) t =
  { cond: Expression.t
  ; t_branch: ('meta, 'stm) Block.t
  ; f_branch: ('meta, 'stm) Block.t }
[@@deriving sexp, make, accessors, compare, equal]

val branch :
  bool -> ('i, ('meta, 'stm) Block.t, ('meta, 'stm) t, [< field]) Accessor.t
(** [branch b] is the accessor [t_branch] when [b] is true, and [f_branch]
    otherwise. *)

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
