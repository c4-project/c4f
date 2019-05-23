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

open Core_kernel

module Address = struct
  type t = Int of int | Symbol of Symbol.t [@@deriving sexp, equal]

  let pp f = function
    | Int k ->
        Int.pp f k
    | Symbol sym ->
        Fmt.pf f "$%s" sym

  let to_string : t -> string = function
    | Int k ->
        Int.to_string k
    | Symbol s ->
        s
end

type t =
  | Register_direct of Register.t
  | Register_indirect of {reg: Register.t; offset: Address.t}
  | Heap of Address.t
  | Unknown
[@@deriving sexp, equal]

module type S_predicates = sig
  type t

  val is_stack_pointer : t -> bool

  val as_stack_offset : t -> Address.t option

  val is_stack_offset : t -> bool

  val is_stack_offset_where : t -> f:(Address.t -> bool) -> bool

  val as_heap_symbol : t -> Symbol.t option

  val is_heap_symbol : t -> bool

  val is_heap_symbol_where : t -> f:(Symbol.t -> bool) -> bool

  val is_dereference : t -> t -> bool
end

module Inherit_predicates
    (P : S_predicates)
    (I : Act_utils.Inherit.S_partial with type c := P.t) :
  S_predicates with type t := I.t = struct
  let is_stack_pointer x =
    Option.exists (I.component_opt x) ~f:P.is_stack_pointer

  let as_stack_offset x = Option.(I.component_opt x >>= P.as_stack_offset)

  let is_stack_offset_where x ~f =
    Option.exists (I.component_opt x) ~f:(P.is_stack_offset_where ~f)

  let is_stack_offset x =
    Option.exists (I.component_opt x) ~f:P.is_stack_offset

  let as_heap_symbol x = Option.(I.component_opt x >>= P.as_heap_symbol)

  let is_heap_symbol_where x ~f =
    Option.exists (I.component_opt x) ~f:(P.is_heap_symbol_where ~f)

  let is_heap_symbol x =
    Option.exists (I.component_opt x) ~f:P.is_heap_symbol

  let is_dereference src dst =
    Option.exists
      (Option.both (I.component_opt src) (I.component_opt dst))
      ~f:(Tuple2.uncurry P.is_dereference)
end

module Predicates : S_predicates with type t := t = struct
  let is_stack_pointer = function
    | Register_direct Register.Stack_pointer ->
        true
    | Register_direct _ | Register_indirect _ | Heap _ | Unknown ->
        false

  let as_stack_offset = function
    | Register_indirect {reg= Stack_pointer; offset= k} ->
        Some k
    | Register_indirect _ | Register_direct _ | Heap _ | Unknown ->
        None

  let is_stack_offset_where l ~f = Option.exists ~f (as_stack_offset l)

  let is_stack_offset = is_stack_offset_where ~f:(Fn.const true)

  let as_heap_symbol = function
    | Heap (Symbol s) ->
        Some s
    | Heap _ | Register_indirect _ | Register_direct _ | Unknown ->
        None

  let is_heap_symbol_where l ~f = Option.exists ~f (as_heap_symbol l)

  let is_heap_symbol = is_heap_symbol_where ~f:(Fn.const true)

  let is_dereference src dst =
    match (src, dst) with
    | ( Register_indirect {reg= src_reg; offset= Int 0}
      , Register_direct dest_reg ) ->
        Register.equal src_reg dest_reg
    | _, _ ->
        false
end

include Predicates

let pp f = function
  | Register_direct reg ->
      Format.fprintf f "reg:%a" Register.pp reg
  | Register_indirect {reg; offset= Int 0} ->
      Format.fprintf f "*(reg:%a)" Register.pp reg
  | Register_indirect {reg; offset= Int k} when k < 0 ->
      Format.fprintf f "@[*(@,reg:%a@ -@ %d@,)@]" Register.pp reg
        (Int.abs k)
  | Register_indirect {reg; offset} ->
      Format.fprintf f "@[*(@,reg:%a@ +@ %a@,)@]" Register.pp reg Address.pp
        offset
  | Heap addr ->
      Format.fprintf f "*(heap:%a)" Address.pp addr
  | Unknown ->
      String.pp f "unknown"

module Kind = struct
  module M = struct
    type t = Register_direct | Register_indirect | Heap | Unknown
    [@@deriving sexp, enum]

    let table =
      [ (Register_direct, "register-direct")
      ; (Register_indirect, "register-indirect")
      ; (Heap, "heap")
      ; (Unknown, "unknown") ]
  end

  include M
  include Act_utils.Enum.Extend_table (M)
end

let kind = function
  | Register_direct _ ->
      Kind.Register_direct
  | Register_indirect _ ->
      Register_indirect
  | Heap _ ->
      Heap
  | Unknown ->
      Unknown

module Flag = Flag_enum.None
