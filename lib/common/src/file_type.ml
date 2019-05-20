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
open Act_utils

type t = Assembly | C | C_litmus | Infer [@@deriving sexp]

let file_type_is (src : Io.In_source.t) (expected : string) : bool =
  Option.exists (Io.In_source.file_type src) ~f:(String.equal expected)

let is_c (src : Io.In_source.t) : t -> bool = function
  | C ->
      true
  | Infer ->
      file_type_is src "c"
  | Assembly | C_litmus ->
      false

let is_c_litmus (src : Io.In_source.t) : t -> bool = function
  | C_litmus ->
      true
  | Infer ->
      file_type_is src "litmus"
  | Assembly | C ->
      false

let delitmusified : t -> t = function
  | C_litmus ->
      C
  | (Assembly | C | Infer) as x ->
      x
