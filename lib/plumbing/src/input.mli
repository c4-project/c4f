(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Type of input into a filter or similar construct.

    This module contains a type that describes how a filter should receive
    its input---for example, from a local file, or standard input. *)

open Base
open Stdio

(** Opaque type of inputs. *)
type t

(** @inline *)
include Io_types.Common with type t := t

(** {2 Constructing an input} *)

val file : Fpath.t -> t
(** [file fname] creates an input source for a given filename. *)

val stdin : ?file_type:string -> unit -> t
(** [stdin] is the standard input source.

    If [file_type] is given, it is reported as the file type of standard
    input, in the same way as the extension of a [file]. *)

(** {2 Querying an input} *)

val file_type : t -> string option
(** [file_type src] gets the file type of [src]. This is the file extension
    for [file]s, and the optional provided extension otherwise. *)

(** {2 Consuming an input} *)

val with_input : t -> f:(In_channel.t -> 'a Or_error.t) -> 'a Or_error.t
(** [with_input i ~f] runs [f] connected to the input channel pointed to by
    [i]. *)
