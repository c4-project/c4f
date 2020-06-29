(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Helpers for statement and expression classification. *)

(** Makes the extensions for a class type. *)
module Make_ext (B : Class_types.S) :
  Class_types.S_ext with type t := B.t and type 'meta elt := 'meta B.elt

val lift_classify_rec : ('m -> 't option) -> 'm -> 't list
(** [lift_classify_rec cmp f] lifts a non-recursive classifier to the
    signature for recursive classifiers. This is useful for leaf classifiers. *)
