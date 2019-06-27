(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to permit
   persons to whom the Software is furnished to do so, subject to the
   following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
   NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
   DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
   OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
   USE OR OTHER DEALINGS IN THE SOFTWARE. *)

(** Type of input into a filter or similar construct.

    This module contains a type that describes how a filter should receive
    its input---for example, from a local file, or standard input. *)

open Base
open Stdio

type t
(** Opaque type of inputs. *)

include Io_types.Common with type t := t
(** @inline *)

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
