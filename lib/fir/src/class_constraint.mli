(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Fir: class constraints

    These are used in the [satisfies] function for evaluating whether classes
    match templates or not, and exist in their own module mostly to break a
    dependency cycle between {!Class} and {!Class_types}. *)

(** Type of class constraints. *)
type t = Is | Is_not_any | Is_not_one | Has | Has_not_any | Has_not_one
[@@deriving compare, equal, sexp]
