(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

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

(** Sanitiser: enumeration of passes *)

open Utils

(** [Pass] enumerates the various sanitisation passes. *)
type t =
  (* Run language-specific hooks. *)
  | LangHooks
  (* Mangle symbols to ensure litmus tools can lex them. *)
  | MangleSymbols
  (* Remove program boundaries.  (If this pass isn't active,
     program boundaries are retained even if they're not
     jumped to. *)
  | RemoveBoundaries
  (* Remove elements that have an effect in the assembly, but said
     effect isn't captured in the litmus test. *)
  | RemoveLitmus
  (* Remove elements with no (direct) effect in the assembly. *)
  | RemoveUseless
  (* Simplify elements that aren't directly understandable by
     litmus tools. *)
  | SimplifyLitmus
  (* Warn about things the sanitiser doesn't understand. *)
  | Warn

(** We include the usual enum extensions for [Pass]. *)
include Enum.ExtensionTable with type t := t

(** [explain] collects passes that are useful for explaining an
    assembly file without modifying its semantics too much. *)
val explain : Set.t
