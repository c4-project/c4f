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

open Base

(** [Pathset.t] is a record containing various paths used in the execution
    of [act]. *)
type t

(** [File] contains a file-specific path set. *)
module File : sig
  type ps = t

  type t

  val name : t -> string
  (** [name f] gets the basename of [f]. *)

  val c_path : t -> Fpath.t
  (** [c_path f] gets the path of the C input file for [f]. *)

  val litc_path : t -> Fpath.t
  (** [litc_path f] gets the path of the C/litmus input file for [f]. *)

  val asm_path : t -> Fpath.t
  (** [asm_path f] gets the path of the assembly output file for [f]. *)

  val lita_path : t -> Fpath.t
  (** [lita_path f] gets the path of the assembly litmus output file for
      [f]. *)

  val herdc_path : t -> Fpath.t
  (** [herdc_path f] gets the path of the C/litmus Herd run output file for
      [f]. *)

  val herda_path : t -> Fpath.t
  (** [herda_path f] gets the path of the assembly Herd run output file for
      [f]. *)

  val make : ps -> Fpath.t -> t
  (** [make ps filename] makes file-specific paths for file [filename],
      according to pathset [ps]. *)
end

val mkdirs : t -> unit Or_error.t
(** [mkdirs] tries to make the output directories mentioned in a
    [Pathset.t]. *)

val make :
     Config.Id.t
  -> input_mode:Input_mode.t
  -> output_root:Fpath.t
  -> t Or_error.t
(** [make id ~input_mode ~output_root] constructs a pathset for compiler ID
    [id], with all output relative to [output_root] relative to [out_root],
    and the input determined by [input_mode]. *)

val make_and_mkdirs :
     Config.Id.t
  -> input_mode:Input_mode.t
  -> output_root:Fpath.t
  -> t Or_error.t
(** [make_and_mkdirs] behaves as {{!make} make}, then tries to make the
    directories through [mkdirs]. *)

val input_mode : t -> Input_mode.t
(** [input_mode ps] gets the input mode used to construct [ps]. *)

val to_files : t -> File.t list
(** [to_files ps] constructs a file pathset for each C litmus test in [ps]. *)

include Pretty_printer.S with type t := t
