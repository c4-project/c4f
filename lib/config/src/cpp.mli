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

(** Abstract type of C preprocessor configurations. *)

(** Opaque CPP configuration type. *)
type t [@@deriving sexp]

(** {2 Constructors} *)

val make : enabled:bool -> ?cmd:string -> ?argv:string list -> unit -> t
(** [make ~enabled ?cmd ?argv ()] makes a CPP configuration block. *)

(** {2 Accessors} *)

val default : unit -> t
(** [default ()] gets the default configuration for the C preprocessor. *)

val enabled : t -> bool
(** [enabled cfg] gets whether this program is enabled, according to [cfg]. *)

val cmd : t -> string
(** [cmd cfg] gets the configured command. *)

val argv : t -> string list
(** [argv cfg] gets the configured arguments. *)
