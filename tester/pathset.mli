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

(** [Run] contains a compiler-agnostic path set for one test run. *)
module Run : sig
  type t

  (** {3 Constructors} *)

  (** Type of input to the constructors. *)
  type input = {input_mode: Input_mode.t; output_root_dir: Fpath.t}

  val make : input -> t Or_error.t
  (** [make {input_mode; output_root_dir}] tries to construct a pathset for
      a tester run with input mode [input_mode], and all output relative to
      [output_root_dir]. *)

  val make_and_mkdirs : input -> t Or_error.t
  (** [make_and_mkdirs in] behaves as [make in], then tries to make the
      output root directory and compiler-agnostic output subdirectories, if
      possible. *)

  (** {3 Accessors} *)

  val c_litmus_files : t -> Fpath.t list
  (** [c_litmus_files ps] gets a list of all C/litmus input files contained
      in [ps]. *)

  val input_mode : t -> Input_mode.t
  (** [input_mode ps] gets the input mode used to construct [ps]. *)

  val output_root_dir : t -> Fpath.t
  (** [output_root_dir cfg] gets the output root directory for [ps]. *)

  val c_sim_file : t -> string -> Fpath.t
  (** [c_sim_file ps f] gets the path of the C/litmus simulator run output
      file for [f] according to [ps]. *)

  include Pretty_printer.S with type t := t
end

module Compiler : sig
  type t

  (** {3 Constructors} *)

  (** Type of input to the constructors. *)
  type input = {run: Run.t; compiler_id: Config.Id.t}

  val make : input -> t Or_error.t
  (** [make {run; compiler_id}] constructs a pathset for compiler ID
      [compiler_id], relative to the output and input paths contained in
      [run]. *)

  val make_and_mkdirs : input -> t Or_error.t
  (** [make_and_mkdirs in] behaves as [make in], then tries to make the
      output root directory and output subdirectories, if possible. *)

  (** {3 Accessors} *)

  val c_litmus_files : t -> Fpath.t list
  (** [c_litmus_files ps] gets a list of all C/litmus input files contained
      in [ps]. *)

  val input_mode : t -> Input_mode.t
  (** [input_mode ps] gets the input mode used to construct [ps]. *)

  val output_root_dir : t -> Fpath.t
  (** [output_root_dir cfg] gets the output root directory for [ps]. *)

  include Pretty_printer.S with type t := t
end

(** [File] contains a file-specific path set. *)
module File : sig
  type t

  (** {2 Constructors} *)

  val make : Compiler.t -> Fpath.t -> t
  (** [make ps filename] makes file-specific paths for file [filename],
      according to compiler pathset [ps]. *)

  val make_all : Compiler.t -> t list
  (** [make_all ps] makes file pathsets for every C litmus test in [ps]. *)

  (** {2 Accessors} *)

  val name : t -> string
  (** [name f] gets the basename of [f]. *)

  val c_file : t -> Fpath.t
  (** [c_file f] gets the path of the C input file for [f]. *)

  val c_litmus_file : t -> Fpath.t
  (** [c_litmus_file f] gets the path of the C/litmus input file for [f]. *)

  val asm_file : t -> Fpath.t
  (** [asm_file f] gets the path of the assembly output file for [f]. *)

  val asm_litmus_file : t -> Fpath.t
  (** [asm_litmus_file f] gets the path of the assembly litmus output file
      for [f]. *)

  val c_sim_file : t -> Fpath.t
  (** [c_sim_file f] gets the path of the C/litmus simulator run output file
      for [f]. *)

  val asm_sim_file : t -> Fpath.t
  (** [asm_sim_file f] gets the path of the assembly simulator run output
      file for [f]. *)
end
