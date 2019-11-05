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

open Base

type t = Int of int | Symbol of Symbol.t [@@deriving sexp, equal]

let pp f = function Int k -> Int.pp f k | Symbol sym -> Fmt.pf f "$%s" sym

let to_string : t -> string = function
  | Int k ->
      Int.to_string k
  | Symbol s ->
      s

module Kind = struct
  module M = struct
    type t = Int | Symbol [@@deriving sexp, enum]

    let table = [(Int, "int"); (Symbol, "symbol")]
  end

  include M
  include Act_utils.Enum.Extend_table (M)
end

module Flag = Flag_enum.None

let kind : t -> Kind.t = function Int _ -> Int | Symbol _ -> Symbol
