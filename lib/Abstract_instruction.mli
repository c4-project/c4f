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

(** [Abstract_instruction] contains types and utilities for abstracted
    instructions. *)

(** [t] is an abstracted instruction. *)
type t =
  | Arith   (** arithmetic *)
  | Call    (** calling-convention related instructions *)
  | Compare (** comparison *)
  | Fence   (** memory fence *)
  | Jump    (** conditional or unconditional jump *)
  | Logical (** logical operation *)
  | Move    (** move *)
  | Nop     (** no operation *)
  | Return  (** jump to caller *)
  | Rmw     (** read-modify-write *)
  | Stack   (** stack resizing and pointer manipulation *)
  | Other   (** known, but doesn't fit in these categories *)
  | Unknown (** unclassified instruction *)
;;

(** [with_operands] is an abstracted instruction with operand
    bundle attached. *)
type with_operands [@@deriving sexp]

(** [opcode] gets the opcode of a [with_operands]. *)
val opcode : with_operands -> t
(** [operands] gets the operands of a [with_operands]. *)
val operands : with_operands -> Abstract_operands.t
(** [make_with_operands ~opcode ~operands] makes a [with_operands]
   from an [opcode] and an [operands] bundle. *)
val make_with_operands
  :  opcode:t
  -> operands:Abstract_operands.t
  -> with_operands
;;

(* Why do we have a separate [Return] type, instead of classing it
   as [Call] or [Jump]?  Two reasons:

   - It has semantics roughly in between both;

   - It makes it easier for us to translate returns to
   end-of-program jumps in sanitisation.  *)

include Abstract_base.S_enum with type t := t

(** [S_predicates] is the signature of any module that can access
    simple predicates over an abstract instruction. *)
module type S_predicates = sig
  (** [t] is the type we're querying. *)
  type t

  (** [is_jump ins] tests whether [ins] is a jump operation. *)
  val is_jump : t -> bool

  (** [is_symbolic_jump ins] tests whether [ins] is a jump to a
      symbol (either immediate, or using the symbol as a heap
      reference). *)
  val is_symbolic_jump : t -> bool

  (** [is_symbolic_jump_where ins ~f] tests whether [ins] is a jump to
     a symbol whose label matches the predicate [f]. *)
  val is_symbolic_jump_where
    : t -> f:(Abstract_symbol.t -> bool) -> bool
  ;;

  (** [is_nop ins] tests whether the [ins] is a no-operation. *)
  val is_nop : t -> bool

  (** [is_stack_manipulation ins ] tests whether [ins] is manipulating
     the stack pointer. *)
  val is_stack_manipulation : t -> bool
end

(** [Inherit_predicates] generates a [S_properties] by inheriting it
    from an optional component.  Each predicate returns false when the
    component doesn't exist. *)
module Inherit_predicates
  : functor (P : S_predicates)
    -> functor (I : Utils.Inherit.S_partial with type c := P.t)
      -> S_predicates with type t := I.t
;;


(** [S_properties] is the signature of any module that can access
    properties (including predicates) of an abstract instruction. *)
module type S_properties = sig
  (** [t] is the type we're querying. *)
  type t

  (** Anything that can access properties can also access predicates. *)
  include S_predicates with type t := t
end

(** [Inherit_properties] generates a [S_properties] by inheriting it
    from a component. *)
module Inherit_properties
  : functor (P : S_properties)
    -> functor (I : Utils.Inherit.S with type c := P.t)
      -> S_properties with type t := I.t
;;

(** We include the functions provided in [S_properties], but define
    them over [with_operands] rather than [t].  This is because some of
    the operations require operand analysis. *)
include S_properties with type t := with_operands
