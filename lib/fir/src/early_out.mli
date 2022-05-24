(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** FIR: 'early out' statements.

    An 'early out' statement is a {{!Prim_statement} primitive statement}
    that represents an abrupt break in control flow. These are:

    - breaks;
    - continues;
    - returns (FIR doesn't support valued return statements). *)

open Base

(** Type of early-out statements. *)
type t = Break | Continue | Return
[@@deriving sexp, compare, equal, quickcheck]

(** {1 Accessors} *)

val in_loop_only : t -> bool
(** [in_loop_only kind] gets whether [kind] is valid only in loops. *)
