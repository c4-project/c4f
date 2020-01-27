(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Mini-C: 'early out' statements.

    An 'early out' statement is a {{!Prim_statement} primitive statement}
    that represents an abrupt break in control flow. These are:

    - breaks;
    - continues;
    - returns (Mini-C doesn't support valued return statements). *)

open Base

(** Type of early-out statements. *)
type t = Break | Continue | Return
[@@deriving sexp, compare, equal, quickcheck]

(** {1 Accessors} *)

val in_loop_only : t -> bool
(** [in_loop_only kind] gets whether [kind] is valid only in loops. *)
