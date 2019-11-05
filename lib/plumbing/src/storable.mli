(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Functors for constructing {!Storable_types.S} implementations. *)

open Base

(** {1 Functors} *)

(** [Make] extends a [Basic] into an [S]. *)
module Make (B : Storable_types.Basic) : Storable_types.S with type t = B.t

(** {2 Storing to standard formats} *)

(** [Of_sexpable] extends a [Sexpable.S] into an [S]; the added methods store
    S-expressions. *)
module Of_sexpable (B : Sexpable.S) : Storable_types.S with type t = B.t

(** [Of_jsonable] extends a {!Jsonable_types.To} into an [S]; the added
    methods store JSON. *)
module Of_jsonable (B : Jsonable_types.To) :
  Storable_types.S with type t = B.t

(** {2 Chaining} *)

(** Makes a new {{!S} S} from chaining a basic storable [B] to a
    transformation function described in [C]. *)
module Make_chain
    (B : Storable_types.Basic)
    (C : Convert_types.S_with_failure with type dst := B.t) :
  Storable_types.S with type t = C.src
