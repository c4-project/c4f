(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** A lightweight notion of scope used in various bits of ACT. *)

(** Type of scopes. *)
type t = Local of int | Global [@@deriving compare, equal, sexp, accessors]

include Base.Comparable.S with type t := t

include Base.Pretty_printer.S with type t := t

val is_global : t -> bool
(** [is_global x] is true if [x] is global, and false otherwise. *)

val reduce : t * 'a -> t * 'a -> t * 'a
(** [reduce l r] returns [l] if the scope tag of [l] is local and [r] is
    global, [r] if vice versa, or an undefined choice of the two otherwise. *)
