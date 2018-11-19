(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining
   a copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to
   permit persons to whom the Software is furnished to do so, subject to
   the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
   LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
   OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
   WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. *)

(** [Abstract_operands] contains types and utilities for abstracted
    operand bundles. *)

open Base

type common =
  [ (** There was a definite error in the original assembly in
        regards to this operand. *)
    `Erroneous of Error.t
  | (** This operand bundle is known and valid, but doesn't yet have
        an abstract representation. *)
    `Other
  | (** This operand bundle is not yet understood by act. *)
    `Unknown
  ]
[@@deriving sexp]
;;

type common_or_loc =
  [ common
  | `Location of Abstract_location.t
  ]
[@@deriving sexp]
;;

type src =
  [ common_or_loc
  | `Int of int
  | `Symbol of string
  ]
[@@deriving sexp]
;;

type dst = common_or_loc
[@@deriving sexp]
;;

type any = src
[@@deriving sexp]
;;

(** [t] is an abstracted operand bundle. *)
type t =
  [ common
  (** This instruction takes no operands. *)
  | `None
  (** This instruction takes a single operand. *)
  | `Single of any
  (** This instruction takes a source operand and a destination
      operand. *)
  | `Src_dst of (src, dst) Src_dst.t
  (** This instruction takes two operands, but they don't form one of
      the specific patterns mentioned above. *)
  | `Double of any * any
  ]
[@@deriving sexp]

(** [is_part_unknown operands] returns true when any part of the
    bundle [operands] is unknown. *)
val is_part_unknown : t -> bool

(** [errors operands] returns any errors corresponding to bad
    operands in the bundle [operands]. *)
val errors : t -> Error.t list

(** [single operand] constructs an operand bundle for an instruction
    with only one operand [operand]. *)
val single : any -> t

(** [src_dst ~src ~dst] constructs an operand bundle for an
    instruction with a source operand [src] and a destination
    operand [dst]. *)
val src_dst : src:src -> dst:dst -> t

(** [is_src_dst operands] returns [true] if [operands] represents a
    bundle of source and destination operand. *)
val is_src_dst : t -> bool

(** [uses_immediate_heap_symbol operands ~syms] returns [true] if
    [operands] contains a symbol in immediate position that,
    according to [syms], is a heap location.  This can mean that the
    operands' parent instruction is manipulating a heap address, for
    instance. *)
val uses_immediate_heap_symbol
  :  t
  -> syms:Abstract_symbol.Table.t
  -> bool
;;

(** [double operand] constructs an operand bundle for an instruction
    with two operands [op1] and [op2], where the operands aren't
    related in any more specific way (eg [src_dst]). *)
val double : any -> any -> t

include Abstract_base.S with type t := t
