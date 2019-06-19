(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

(** {2 Functors} *)

(** [Make] extends a [Basic] into an [S]. *)
module Make (B : Loadable_types.Basic) : Loadable_types.S with type t = B.t

(** {3 Loading from standard formats} *)

(** [Of_sexpable] extends a [Sexpable.S] into an [S]; the added methods load
    S-expressions. *)
module Of_sexpable (B : Sexpable.S) : Loadable_types.S with type t = B.t

(** [Of_jsonable] extends a [Of_jsonable] into an [S]; the added methods load
    JSON. *)
module Of_jsonable (B : Loadable_types.Of_jsonable) : Loadable_types.S with type t = B.t


(** {3 Chaining} *)

(** Makes a new {{!S} S} from chaining a basic loadable [B] to a
    transformation function described in [C]. *)
module Make_chain
    (B : Loadable_types.Basic)
    (C : Loadable_types.Basic_chain with type src := B.t) :
  Loadable_types.S with type t = C.dst

(** {3 Interoperability with filters} *)

(** Lifts a {{!S} S} to a {{!Plumbing.Filter_types.S} filter} that outputs
    nothing to its target file and returns the loaded data as auxiliary
    output. *)
module To_filter (L : Loadable_types.S) :
  Filter_types.S with type aux_i = unit and type aux_o = L.t
