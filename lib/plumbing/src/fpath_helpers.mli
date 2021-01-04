(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Miscellaneous helpers for dealing with [Fpath]s. *)

open Base

val of_string : string -> Fpath.t Or_error.t
(** [of_string str] is [Fpath.of_string str], but with the error changed to
    an [Or_error.t]. *)

val of_string_option : string option -> Fpath.t option Or_error.t
(** [of_string_option str_opt] lifts [fpath_of_string] over optional strings. *)

val lift_str :
  string option -> f:(Fpath.t -> 'a) -> default:'a -> 'a Or_error.t
(** [lift_str str_opt ~f ~default] is a more general form of
    [of_string_option] that applies [f] over [of_string] of [str_opt] if it
    exists, and returns [default] if it doesn't. *)

val filename_no_ext : Fpath.t -> string
(** [filename_no_ext path] is the filename of [path], less any extension(s). *)
