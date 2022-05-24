(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** FIR: statement traversal.

    Statement traversal is quite a convoluted mess of recursive traversal
    code, so we hold it separately from {!Statement}.

    See also: {!Expression_traverse}. *)

(** {1 Traversing over statements} *)

module Base_map (M : Base.Applicative.S) : sig
  val bmap :
       'm1 Statement.t
    -> prim:
         (   ('m1, Prim_statement.t) With_meta.t
          -> ('m2, Prim_statement.t) With_meta.t M.t )
    -> if_stm:('m1 Statement.If.t -> 'm2 Statement.If.t M.t)
    -> flow:('m1 Statement.Flow_block.t -> 'm2 Statement.Flow_block.t M.t)
    -> 'm2 Statement.t M.t
  (** [bmap x ~prim ~if_stm ~flow] is a basic monadic (for now) traversal
      over [x], using the appropriate action from those given. *)
end

include Statement_types.S_traversable with type 'meta t := 'meta Statement.t

(** {1 Traversing over statement subcomponents} *)

(** Traversing over if statements. *)
module If :
  Statement_types.S_traversable with type 'meta t = 'meta Statement.If.t

(** Traversing over flow blocks. *)
module Flow_block :
  Statement_types.S_traversable
    with type 'meta t = 'meta Statement.Flow_block.t
