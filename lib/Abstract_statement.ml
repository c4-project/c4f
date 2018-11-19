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

open Core_kernel
open Utils

type t =
  | Directive of string
  | Instruction of Abstract_instruction.t
  | Blank
  | Label of string
  | Other
[@@deriving sexp]

let pp f = function
  | Blank         -> ()
  | Directive   d -> Format.fprintf          f "directive@ (%s)" d
  | Label       l -> Format.fprintf          f ":%s"             l
  | Instruction i -> Abstract_instruction.pp f i
  | Other         -> String.pp               f "??"

module Flag = struct
  module M = struct
    type t =
      [ `UnusedLabel
      | `ProgBoundary
      | `StackManip
      ] [@@deriving enum, sexp]

    let table =
      [ `UnusedLabel, "unused label"
      ; `ProgBoundary, "program boundary"
      ; `StackManip, "manipulates stack"
      ]
  end

  include M
  include Enum.Extend_table (M)
end
