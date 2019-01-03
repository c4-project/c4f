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

(** Modules for building 'filters', in the UNIX sense.

    A filter is a process that takes input from one file and returns
    output in another.  This module contains signatures and functors
    for building and composing filters over the [Io] abstractions. *)

open Base

include module type of Filter_intf

val lift_to_raw_strings
  :  f:('i -> Io.In_source.t -> Io.Out_sink.t -> 'o Or_error.t)
  -> 'i
  -> infile:string option
  -> outfile:string option
  -> 'o Or_error.t
;;

val lift_to_fpaths
  :  f:('i -> Io.In_source.t -> Io.Out_sink.t -> 'o Or_error.t)
  -> 'i
  -> infile:Fpath.t option
  -> outfile:Fpath.t option
  -> 'o Or_error.t
;;

module Make (B : Basic) : S with type aux_i = B.aux_i
                             and type aux_o = B.aux_o
(** Makes a filter from a {{!Basic}Basic}. *)

module Make_in_file_only (B : Basic_in_file_only)
  : S with type aux_i = B.aux_i and type aux_o = B.aux_o
(** Makes a filter from a {{!Basic_in_file_only}Basic_in_file_only}. *)

module Make_files_only (B : Basic_files_only)
  : S with type aux_i = B.aux_i and type aux_o = B.aux_o
(** Makes a filter from a {{!Basic_in_file_only}Basic_files_only}. *)

module Chain (A : S) (B : S) : S with type aux_i = (A.aux_i * B.aux_i)
                                  and type aux_o = (A.aux_o * B.aux_o)
(** Chains two filters together using temporary files. *)

module Chain_conditional_first (B : Basic_chain_conditional)
  : S with type aux_i = (B.First.aux_i        * B.Second.aux_i)
       and type aux_o = (B.First.aux_o option * B.Second.aux_o)
(** Chains an optional filter onto a mandatory one. *)

module Chain_conditional_second (B : Basic_chain_conditional)
  : S with type aux_i = (B.First.aux_i * B.Second.aux_i       )
       and type aux_o = (B.First.aux_o * B.Second.aux_o option)
(** Chains a mandatory filter onto an optional one. *)
