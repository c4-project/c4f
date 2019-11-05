(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the "Software"),
   to deal in the Software without restriction, including without limitation
   the rights to use, copy, modify, merge, publish, distribute, sublicense,
   and/or sell copies of the Software, and to permit persons to whom the
   Software is furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
   THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
   DEALINGS IN THE SOFTWARE. *)

(** Type of output into a filter or similar construct.

    This module contains a type that describes how a filter should dispose of
    its output---for example, into a local file, or on standard output. *)

open Base
open Stdio

type t
(** Opaque type of outputs. *)

include Io_types.Common with type t := t
(** @inline *)

(** {2 Constructing an output} *)

val file : Fpath.t -> t
(** [file fname] creates an output sink for a given filename. *)

val stdout : t
(** [stdout] is the standard output sink. *)

val temp : prefix:string -> ext:string -> t
(** [temp ~prefix ~ext] creates an output sink for a temporary file with the
    given prefix and extension. *)

(** {2 Consuming an output} *)

val as_input : t -> Input.t Or_error.t
(** [as_input o] tries to get an {{!Input.t} input} pointing to the same data
    as [o]. *)

val with_output : t -> f:(Out_channel.t -> 'a Or_error.t) -> 'a Or_error.t
(** [with_output o ~f] runs [f] connected to the output channel pointed to by
    [o]. *)
