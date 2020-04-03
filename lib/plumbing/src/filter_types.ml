(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Signatures used in {!Filter}. *)

open Stdio
open Base

(** Types and values common to both the basic and full filter signatures. *)
module type Common = sig
  (** Type of any auxiliary state consumed by this filter. *)
  type aux_i

  (** Type of any auxiliary state built by this filter. *)
  type aux_o

  val name : string
  (** [name] is the name of this filter. *)
end

module type Basic = sig
  include Common

  val run :
       aux_i Filter_context.t
    -> In_channel.t
    -> Out_channel.t
    -> aux_o Or_error.t
end

(** Input signature for filters that require physical files in input
    position. *)
module type Basic_in_file_only = sig
  include Common

  val run :
    aux_i Filter_context.t -> Fpath.t -> Out_channel.t -> aux_o Or_error.t
end

(** Input signature for filters that require physical files in both input and
    output position. *)
module type Basic_files_only = sig
  include Common

  val run :
       aux_i Filter_context.t
    -> infile:Fpath.t
    -> outfile:Fpath.t
    -> aux_o Or_error.t
end

module type S = sig
  include Common

  val run : aux_i -> Input.t -> Output.t -> aux_o Or_error.t
  (** [run aux source sink] runs this filter on [source], outputs to [sink],
      reads and returns any auxiliary state on success. *)
end

(** Signature of inputs needed to adapt a filter. *)
module type Basic_adapt = sig
  (** The original filter. *)
  module Original : S

  (** The new input type. *)
  type aux_i

  (** The new output type. *)
  type aux_o

  val adapt_i : aux_i -> Original.aux_i Or_error.t
  (** [adapt_i aux] tries to adapt the new input type to the old one. *)

  val adapt_o : Original.aux_o -> aux_o Or_error.t
  (** [adapt_i aux] tries to adapt the old output type to the new one. *)
end

(** Basic signature for building filters from external programs on top of a
    [Runner]. *)
module type Basic_on_runner = sig
  include Common with type aux_o := unit

  (** The runner to use to run the program. *)
  module Runner : Runner_types.S

  val prog : aux_i -> string
  (** [prog aux] gets the name of the program to run, given the auxiliary
      input [aux]. *)

  val argv : aux_i -> string -> string list Or_error.t
  (** [argv aux file] gets the argument vector to supply, given the auxiliary
      input [aux] and input file [file]. It can fail if the inputs given do
      not correspond to a valid invocation of the program. *)
end
