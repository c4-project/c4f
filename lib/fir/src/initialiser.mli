(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** FIR: initialisers

    In FIR, all initialisers must have a value; this is to simplify against
    uninitialised value UB. *)

(** Opaque type of initialisers. *)
type t = {ty: Type.t; value: Constant.t}
[@@deriving sexp, accessors, compare, equal, quickcheck]

(** {1 Shortcuts} *)

val of_int : ?is_atomic:bool -> ?is_volatile:bool -> int -> t
(** [of_int ?is_atomic ?is_volatile value] makes an initialiser for an
    integer with value [value]. If [is_atomic] is present and true, the type
    will be atomic_int; otherwise, it will be int. If [is_volatile] is
    present and true, the type will be volatile. *)
