(* This file is part of 'act'.

Copyright (c) 2018 by Matt Windsor

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. *)

(** Various input/output helpers *)

open Core

(** [Dir] contains high-ish-level operations on directories. *)
module Dir : sig
  (** [get_files ~ext path] wraps [Sys.readfiles] with error handling
      and optional extension filtering. *)
  val get_files
    :  ?ext:string
    -> string
    -> string list Or_error.t
end

(** [CommonS] describes operations common to both input sources and
   output sinks. *)
module type CommonS = sig
  type t

  (** [of_option n] treats [n] as a filename if it's [Some], and
      as a standard stream otherwise. *)
  val of_option : string option -> t;;

  (** [file t] returns [Some f] if [t] is a file with path [f],
      and [None] otherwise. *)
  val file : t -> string option;;
end

(** [In_source] describes input sources. *)
module In_source : sig
  type t =
    [ `File of string
    | `Stdin
    ]

  include Pretty_printer.S with type t := t
  include Sexpable.S with type t := t
  include CommonS with type t := t

  (** [with_input ~f iname] runs [f] connected to the input channel
      pointed to by [iname]. *)
  val with_input
    :  f:(t -> In_channel.t -> 'a Or_error.t)
    -> t
    -> 'a Or_error.t
end

(** [Out_sink] describes output sinks. *)
module Out_sink : sig
  type t =
    [ `File of string
    | `Stdout
    ]

  include Pretty_printer.S with type t := t
  include Sexpable.S with type t := t
  include CommonS with type t := t

  (** [with_output ~f oname] runs [f] connected to the output channel
      pointed to by [oname]. *)
  val with_output
    :  f:(t -> Out_channel.t -> 'a Or_error.t)
    -> t
    -> 'a Or_error.t
end

(** [with_input_and_output f i o] runs [f] with the appropriate
    channels pointed to by [i] and [o]. *)
val with_input_and_output
  :  f:(In_source.t -> In_channel.t ->
        Out_sink.t -> Out_channel.t ->
        'a Or_error.t)
  -> In_source.t
  -> Out_sink.t
  -> 'a Or_error.t
;;
