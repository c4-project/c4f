(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

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

(** Sanitiser passes for global symbol renaming

    This module provides a global sanitiser pass that performs two renaming
    sub-passes on all symbols:

    {ul
     {- [`Unmangle_symbols]: replace compiler-mangled symbols with their
        original C identifiers where doing so is unambiguous;}
     {- [`Escape_symbols]: replace characters in symbols that are difficult
        for Herd-like programs to parse with less human-readable (but more
        machine-readable) equivalents.}} *)

module Make (B : Common.Basic) :
  Common.S_all
  with module Lang := B.Lang
   and module Ctx := B.Ctx
   and module Program_container := B.Program_container
