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
  | StackPointer
  | StackOffset of int
  | Heap of string
  | GeneralRegister
  | Unknown
[@@deriving sexp, eq]
;;

let pp f = function
  | StackPointer      -> String.pp      f "&stack"
  | StackOffset     i -> Format.fprintf f "stack[%d]" i
  | Heap            s -> Format.fprintf f "heap[%s]" s
  | GeneralRegister   -> String.pp      f "reg"
  | Unknown           -> String.pp      f "??"
;;

module Kind = struct
  module M = struct
    type t =
      | Stack_pointer
      | Stack_offset
      | Heap
      | General_register
      | Unknown
    [@@deriving sexp, enum]
    ;;

    let table =
      [ Stack_pointer   , "stack pointer"
      ; Stack_offset    , "stack offset"
      ; Heap            , "heap"
      ; General_register, "general register"
      ; Unknown         , "unknown"
      ]
    ;;
  end

  include M
  include Enum.Extend_table (M)
end

let kind = function
  | StackPointer      -> Kind.Stack_pointer
  | StackOffset     _ -> Stack_offset
  | Heap            _ -> Heap
  | GeneralRegister   -> General_register
  | Unknown           -> Unknown
;;

module Flag = Abstract_flag.None
