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

(** Modules for building 'filters', in the UNIX sense.

    A filter is a process that takes input from one file and returns output
    in another. This module contains signatures and functors for building and
    composing filters over the [Io] abstractions. *)

open Base
open Filter_types

(** {2 Making filters} *)

(** Makes a filter from a {{!Basic} Basic}. *)
module Make (B : Basic) :
  S with type aux_i = B.aux_i and type aux_o = B.aux_o

(** Makes a filter from a {{!Basic_in_file_only} Basic_in_file_only}. *)
module Make_in_file_only (B : Basic_in_file_only) :
  S with type aux_i = B.aux_i and type aux_o = B.aux_o

(** Makes a filter from a {{!Basic_in_file_only} Basic_files_only}. *)
module Make_files_only (B : Basic_files_only) :
  S with type aux_i = B.aux_i and type aux_o = B.aux_o

(** Makes an in-file-only filter by combining a {{!Runner.S} runner} and some
    program accessors. *)
module Make_on_runner (R : Basic_on_runner) :
  S with type aux_i = R.aux_i and type aux_o = unit

(** {2 Adapting filters}

    Filter chaining is in {{!Filter_chain} another module}. *)

(** Adapts the auxiliary types of an existing filter, performing partial
    mappings to and from a new pair of auxiliary types. *)
module Adapt (B : Basic_adapt) :
  S with type aux_i = B.aux_i and type aux_o = B.aux_o
