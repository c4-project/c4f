(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

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

(** Various input/output helpers *)

open Core

include module type of Io_intf

(** [Dir] contains high-ish-level operations on directories. *)
module Dir : sig
  val get_files
    :  ?compare:(Fpath.t -> Fpath.t -> int)
    -> ?ext:string
    -> Fpath.t
    -> Fpath.t list Or_error.t
  (** [get_files ?compare ?ext path] wraps [Sys.readfiles] with error
     handling, optional extension filtering, and path sorting using
      [compare] (which, by default, is ascending collation). *)
end

(** [In_source] describes input sources. *)
module In_source : sig
  type t
  (** The opaque type of input sources. *)

  val file : Fpath.t -> t
  (** [file fname] creates an input source for a given filename. *)

  val fd : ?file_type:string -> Unix.File_descr.t -> t
  (** [fd ?file_type fildes] creates an input source for a given file
     descriptor.

      If [file_type] is given, it is reported as the file type of the
     descriptor, in the same way as the extension of a [file]. *)

  val stdin : ?file_type:string -> unit -> t
  (** [stdin] is the standard input source.

      If [file_type] is given, it is reported as the file type of
     standard input, in the same way as the extension of a [file]. *)

  val file_type : t -> string option
  (** [file_type src] gets the file type of [src].  This is the
      file extension for [file]s, and the optional provided extension
      otherwise. *)

  include Common with type t := t

  val with_input
    :  t
    -> f:(t -> In_channel.t -> 'a Or_error.t)
    -> 'a Or_error.t
  (** [with_input iname ~f] runs [f] connected to the input channel
      pointed to by [iname]. *)
end

(** [Out_sink] describes output sinks. *)
module Out_sink : sig
  type t
  (** The opaque type of output sinks. *)

  val file : Fpath.t -> t
  (** [file fname] creates an output sink for a given filename. *)

  val fd : Unix.File_descr.t -> t
  (** [fd fildes] creates an output sink for a given file descriptor. *)

  val stdout : t
  (** [stdout] is the standard output sink. *)

  val temp : prefix:string -> ext:string -> t
  (** [temp ~prefix ~ext] creates an output sink for a temporary
      file with the given prefix and extension. *)

  val as_in_source : t -> In_source.t Or_error.t
  (** [as_in_source sink] tries to get an input source pointing to the
      same data as [sink]. *)

  include Common with type t := t

  val with_output
    :  t
    -> f:(t -> Out_channel.t -> 'a Or_error.t)
    -> 'a Or_error.t
  (** [with_output oname ~f] runs [f] connected to the output channel
      pointed to by [oname].  It returns the result of [f]. *)
end

val fpath_of_string : string -> Fpath.t Or_error.t
(** [fpath_of_string str] is [Fpath.of_string str], but with the
    error changed to an [Or_error.t]. *)

val fpath_of_string_option : string option -> Fpath.t option Or_error.t
(** [fpath_of_string_option str_opt] lifts [fpath_of_string] over
    optional strings. *)

val with_input_and_output
  :  In_source.t
  -> Out_sink.t
  -> f:(In_source.t -> In_channel.t ->
        Out_sink.t -> Out_channel.t ->
        'a Or_error.t)
  -> 'a Or_error.t
(** [with_input_and_output i o ~f] runs [f] with the appropriate
    channels pointed to by [i] and [o]. *)

val print_bool : bool -> unit
(** [print_bool b] prints the truth value of [b] to stdout. *)
