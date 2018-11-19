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

open Utils

module M = struct
  type t =
    | Arith
    | Call
    | Compare
    | Fence
    | Jump
    | Logical
    | Move
    | Nop
    | Return
    | Rmw
    | Stack
    | Other
    | Unknown
  [@@deriving enum, sexp]
  ;;

  let table =
    [ Arith  , "arith"
    ; Call   , "call"
    ; Compare, "compare"
    ; Fence  , "fence"
    ; Jump   , "jump"
    ; Logical, "logical"
    ; Move   , "move"
    ; Nop    , "nop"
    ; Return , "return"
    ; Rmw    , "RMW"
    ; Stack  , "stack"
    ; Other  , "other"
    ; Unknown, "??"
    ]
end

include M
include Enum.Extend_table (M)
module Flag = Abstract_flag.None
