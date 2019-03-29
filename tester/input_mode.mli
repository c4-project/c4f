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

(** Input modes for the tester.

    The tester supports reading in litmus tests (and, optionally, pre-built
    compilable C files) in several different ways. This module contains the
    means for selecting such input modes; its results are then used to build
    {{!Pathset} pathsets}. *)

open Base

(** Opaque type of input modes. *)
type t

(** {2 Constructors} *)

val memalloy : input_root:Fpath.t -> t Or_error.t
(** [memalloy ~input_root] creates, and validates, a Memalloy input mode
    record using the given input root. *)

val litmus_only : files:Fpath.t list -> t Or_error.t
(** [litmus_only ~files] creates, and validates, a litmus-only input mode
    record using the given files. *)

(** {2 Queries} *)

val reduce :
  t -> memalloy:(Fpath.t -> 'a) -> litmus_only:(Fpath.t list -> 'a) -> 'a
(** [reduce imode ~memalloy ~litmus_only] destructs [imode], applying the
    appropriate reduction function to the mode. *)

val must_delitmusify : t -> bool
(** [must_delitmusify imode] returns [true] if the tester must generate C
    files by de-litmusifying the C litmus tests, or [false] if they are
    already assumed to exist. *)
