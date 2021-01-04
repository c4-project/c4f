(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Expression traversal.

    All traversal has to be done centrally for expressions, because OCaml
    gets very persnickety (and not very helpful) when it comes to mutually
    recursive functors. *)

(** {1 Traversing over expressions} *)

include Expression_types.S_traversable with type t := Expression.t

(** {1 Traversing over expression subcomponents} *)

(** Traversing over compare-exchange. *)
module Cmpxchg :
  Expression_types.S_traversable with type t = Expression.t Atomic_cmpxchg.t

(** Traversing over fetch. *)
module Fetch :
  Expression_types.S_traversable with type t = Expression.t Atomic_fetch.t

(** Traversing over exchange. *)
module Xchg :
  Expression_types.S_traversable with type t = Expression.t Atomic_xchg.t

(** Traversing over atomic expressions. *)
module Atomic :
  Expression_types.S_traversable
    with type t = Expression.t Atomic_expression.t

(** {1 Accessors}

    These mainly abstract over the traversals above, for now. *)

val depended_upon_idents :
  ( 'i
  , Act_common.C_id.t
  , Expression.t
  , [< Accessor.many_getter] )
  Accessor.Simple.t
(** [depended_upon_idents] recursively gets every identifier in an expression
    that said expression is considered to depend upon. Over time, the notion
    of dependence may weaken, and this getter may pull in a subset of the
    identifiers mentioned in the expression. *)
