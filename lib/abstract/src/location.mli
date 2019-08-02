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

(** Abstract program model: types and functions for locations. *)

open Base

(** [t] is an abstracted location. *)
type t =
  | Register_direct of Register.t
  | Register_indirect of {reg: Register.t; offset: Address.t}
  | Heap of Address.t
  | Unknown
[@@deriving sexp, equal]

(** [S_predicates] is the signature of any module that can access simple
    predicates over an abstract location. *)
module type S_predicates = sig
  type t
  (** [t] is the type we're querying. *)

  val is_stack_pointer : t -> bool
  (** [is_stack_pointer loc] tests whether [loc] is a direct reference to a
      stack pointer. *)

  val as_stack_offset : t -> Address.t option
  (** [as_stack_offset loc] returns [Some k] when [loc] is a
      register-indirect adding an offset [k] to the stack pointer, and
      [None] otherwise. *)

  val is_stack_offset : t -> bool
  (** [is_stack_offset loc ~f] tests whether [loc] is a stack offset. *)

  val is_stack_offset_where : t -> f:(Address.t -> bool) -> bool
  (** [is_stack_offset_where loc ~f] tests whether [loc] is a stack offset
      whose offset satisfies [f]. *)

  val as_heap_symbol : t -> Symbol.t option
  (** [as_heap_symbol loc] returns [Some k] when [loc] is a symbolic heap
      address, and [None] otherwise. *)

  val is_heap_symbol : t -> bool
  (** [is_heap_symbol loc ~f] tests whether [loc] is a heap symbol. *)

  val is_heap_symbol_where : t -> f:(Symbol.t -> bool) -> bool
  (** [is_heap_symbol_where loc ~f] tests whether [loc] is a heap symbol
      whose offset satisfies [f]. *)

  val is_dereference : t -> t -> bool
  (** [is_dereference src dst] tests whether [src] is an indirect location
      routing through a location equal to [dst]. *)
end

module Inherit_predicates
    (P : S_predicates)
    (I : Act_utils.Inherit_types.S_partial with type c := P.t) :
  S_predicates with type t := I.t
(** [Inherit_predicates] generates a [S_predicates] by inheriting it from an
    optional component. Each predicate returns false when the component
    doesn't exist. *)

module Kind : sig
  type t = Register_direct | Register_indirect | Heap | Unknown

  include Act_utils.Enum_types.S_table with type t := t

  include Act_utils.Enum_types.Extension_table with type t := t
end

include
  S_predicates with type t := t
(** This module contains [S_predicates] directly. *)

include Node.S with type t := t and module Kind := Kind
