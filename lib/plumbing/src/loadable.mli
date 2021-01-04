(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Functors for constructing {!Loadable_types.S} implementations. *)

open Base

(** {1 Functors} *)

(** [Make] extends a [Basic] into an [S]. *)
module Make (B : Loadable_types.Basic) : Loadable_types.S with type t = B.t

(** {2 Loading from standard formats} *)

(** [Of_sexpable] extends a [Sexpable.S] into an [S]; the added methods load
    S-expressions. *)
module Of_sexpable (B : Sexpable.S) : Loadable_types.S with type t = B.t

(** [Of_jsonable] extends a {!Jsonable_types.Of} into an [S]; the added
    methods load JSON. *)
module Of_jsonable (B : Jsonable_types.Of) :
  Loadable_types.S with type t = B.t

(** {2 Chaining} *)

(** Makes a new {!S} from chaining a basic loadable [B] to a transformation
    function described in [C]. *)
module Make_chain
    (B : Loadable_types.Basic)
    (C : Convert_types.S_with_failure with type src := B.t) :
  Loadable_types.S with type t = C.dst
