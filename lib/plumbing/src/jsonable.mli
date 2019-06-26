(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Helper functors for building {{!Jsonable_types} JSONable} modules. *)

open Base

module Make_alist (K : Stringable.S) (V : Jsonable_types.S) :
  Jsonable_types.S with type t = (K.t, V.t) List.Assoc.t

module Make_map (K : sig
  type t

  include Comparable.S with type t := t

  include Stringable.S with type t := t
end)
(V : Jsonable_types.S) : Jsonable_types.S with type t = V.t Map.M(K).t

(** Lifts stringable types to json-able types, where conversion is to/from strings. *)
module Of_stringable (E : Stringable.S) : Jsonable_types.S with type t = E.t

(** Strings packaged up as a json-able type. *)
module String : Jsonable_types.S with type t = string

(** Lifts a json-able type over options. *)
module Option (B : Jsonable_types.S) :
  Jsonable_types.S with type t = B.t option
