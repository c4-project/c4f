(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
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
