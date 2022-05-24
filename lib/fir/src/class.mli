(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Helpers for statement and expression classification.

    (The name 'class' nods to the fact that, were ACT implemented in an
    object oriented language, these enumerations would literally be the class
    hierarchy of the expression and statement type systems.)

    New classes will be added as and when other parts of ACT depend on them.
    To allow this to happen smoothly, the various [classify] functions can
    choose to return [None] when asked for specific information. *)

(** Makes the extensions for a class type. *)
module Make_ext (B : Class_types.S) :
  Class_types.S_ext with type t := B.t and type 'meta elt := 'meta B.elt

val lift_classify_rec : ('m -> 't option) -> 'm -> 't list
(** [lift_classify_rec cmp f] lifts a non-recursive classifier to the
    signature for recursive classifiers. This is useful for leaf classifiers. *)
