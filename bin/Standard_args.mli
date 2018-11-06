(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation
   files (the "Software"), to deal in the Software without
   restriction, including without limitation the rights to use, copy,
   modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

(** [Standard_args] contains argument specifications common to all act
   sub-commands. *)

open Core

(** [t] collects all of the standard arguments in one record. *)
type t

(** [is_verbose t] gets whether, according to [t], verbose mode is
   switched on. *)
val is_verbose : t -> bool

(** [are_warnings_enabled t] gets whether, according to [t], warnings are
   switched on. *)
val are_warnings_enabled : t -> bool

(** [spec_file t] gets the specification file according to [t]. *)
val spec_file : t -> string

(** [get] is a [Command.Param.t] that describes how to get the standard
    arguments at the command line. *)
val get : t Command.Param.t

(** [Other] collects parameter specifications for arguments that aren't
    as common as the standard ones, but are still used in more than
    one sub-command. *)
module Other : sig
  (** [local_only] defines a parameter that can be used to make the
      configuration loader disable compilers on non-local machines. *)
  val local_only : bool Command.Param.t
end
