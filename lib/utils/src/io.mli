(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Various input/output helpers, mostly used for testing. *)

val print_bool : bool -> unit
(** [print_bool b] prints the truth value of [b] to stdout. *)

val prn : 'a Fmt.t -> 'a -> unit
(** [prn pp x] prints [x] to stdout using [pp], followed by a hard newline. *)
