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

(** Top-level language modules for x86 *)

open Base
open Act_common

module Att : Language_definition_types.S
(** [Att] is a language description for the AT&T dialect of x86. *)

module Gcc : Language_definition_types.S
(** [Gcc] is a language description for the GCC inline-assembly dialect of
    x86. *)

module Intel : Language_definition_types.S
(** [Intel] is a language description for the Intel dialect of x86. *)

module Herd7 : Language_definition_types.S
(** [Herd7] is a language description for the Herd7 dialect of x86. *)

val of_dialect : Id.t -> (module Language_definition_types.S) Or_error.t
(** [of_dialect id] gets the correct [S] module for the dialect with ID
    [id].

    If one doesn't exist, [of_dialect] returns an error. *)
