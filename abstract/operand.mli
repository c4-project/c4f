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
    operands and operand bundles. *)

open Base
open Utils

(** [t] is the type of single operands. *)
type t =
  | Int of int
  | Location of Location.t
  | Symbol of Symbol.t
  | Erroneous of Error.t
  (** This operand appears to be incorrect: an error message is
      enclosed in S-expression form. *)
  | Other
  (** This operand is known and valid, but doesn't yet have an
     abstract representation. *)
  | Unknown
  (** This operand is not yet understood by act. *)
[@@deriving sexp, eq]
;;

(** [S_predicates] is the signature of any module that can access
    simple predicates over an abstract statement. *)
module type S_predicates = sig
  type t
  (** [t] is the type we're querying. *)

  include Location.S_predicates with type t := t
  (** Any predicate on a location also works on an operand; it
      responds negatively if the operand isn't a location. *)

  val as_location : t -> Location.t option
  (** [as_location operand] returns [Some loc] if [operand] is a
      location with value [loc], and [None] otherwise. *)

  val is_unknown : t -> bool
  (** [is_unknown operand] tests whether [operand] is unknown (has no
     abstract representation). *)

  val is_immediate : t -> bool
  (** [is_immediate operand] tests whether [operand] is an immediate
      value (an integer or symbol, etc.). *)

  val is_immediate_heap_symbol
    :  t
    -> symbol_table:Symbol.Table.t
    -> bool
  (** [is_immediate_heap_symbol operand ~symbol_table] returns [true]
     if any [operand] is a symbol in immediate position that,
     according to [syms], is a heap location.  This can mean that the
     operand's parent instruction is manipulating a heap address, for
     instance. *)

  val is_jump_symbol : t -> bool
  (** [is_jump_symbol operand] tests whether [operand] is a possible
     symbolic jump target (an immediate symbol or heap location). *)

  val is_jump_symbol_where
    :  t
    -> f:(Symbol.t -> bool)
    -> bool
  (** [is_jump_symbol_where operand ~f] tests whether [operand] is a
      possible symbolic jump target (an immediate symbol or heap
      location), and, if so, whether it matches predicate [f]. *)
end

(** [Inherit_predicates] generates a [S_predicates] by inheriting it
    from an optional component.  Each predicate returns false when the
    component doesn't exist. *)
module Inherit_predicates
  : functor (P : S_predicates)
    -> functor (I : Utils.Inherit.S_partial with type c := P.t)
      -> S_predicates with type t := I.t
;;

(** [Flag] is an enumeration of various single-operand observations. *)
module Flag : sig
  type t =
    [ `Stack_pointer
    | `Jump_symbol
    | `Immediate_heap_symbol
    ]
  [@@deriving sexp, enumerate]
  ;;

  include Flag_enum.S with type t := t
end

(** [S_properties] is the signature of any module that can access
    properties (including predicates) of an abstract operand. *)
module type S_properties = sig
  type t
  (** [t] is the type we're querying. *)

  include S_predicates with type t := t
  (** Anything that can access properties can also access predicates. *)

  val flags : t -> Symbol.Table.t -> Flag.Set.t
  (** [flags x symbol_table] gets the statement flags for [x] given
      symbol table [symbol_table]. *)
end

(** [Inherit_properties] generates a [S_properties] by inheriting it
    from a component. *)
module Inherit_properties
  : functor (P : S_properties)
    -> functor (I : Utils.Inherit.S with type c := P.t)
      -> S_properties with type t := I.t
;;

(** This module contains [S_properties] directly. *)
include S_properties with type t := t

include Node.S with type t := t and module Flag := Flag

(** [Bundle] is the abstract data type of collections of operands,
    such as those attached to an instruction. *)
module Bundle : sig
  type elt = t
  (** [elt] is a synonym for the single operand type. *)

  type t =
    | None
    | Single of elt
    | Double of elt * elt
    | Src_dst of (elt, elt) Src_dst.t
  [@@deriving sexp]
  (** [t] is an abstracted operand bundle. *)

  (** {3 Constructors} *)

  val single : elt -> t
  (** [single operand] constructs an operand bundle for an instruction
      with only one operand [operand]. *)

  (** [double operand] constructs an operand bundle for an instruction
      with two operands [op1] and [op2], where the operands aren't
      related in any more specific way (eg [src_dst]). *)
  val double : elt -> elt -> t

  (** [src_dst ~src ~dst] constructs an operand bundle for an
      instruction with a source operand [src] and a destination
      operand [dst]. *)
  val src_dst : src:elt -> dst:elt -> t

  (** {3 Predicates} *)

  (** [S_predicates] is the signature of any module that can access
      simple predicates over an operand bundle. *)
  module type S_predicates = sig
    type t
    (** [t] is the type we're querying. *)

    val is_none : t -> bool
    (** [is_none bundle] returns [true] if [bundle] contains no
       operands. *)

    val as_src_dst : t -> (elt, elt) Src_dst.t option
    (** [as_src_dst bundle] returns [Some {src; dst}] if [bundle]
       represents a bundle of source operand [src] and destination
       operand [dst]. *)

    val is_src_dst : t -> bool
    (** [is_src_dst bundle] returns [true] if [bundle] represents a
        bundle of source and destination operand. *)

    val is_part_unknown : t -> bool
    (** [is_part_unknown bundle] returns true when any operand in
        [bundle] is unknown, or the whole bundle's layout is unknown. *)

    val has_stack_pointer : t -> bool
    (** [has_stack_pointer_src bundle] tests whether [bundle]
        contains a stack pointer. *)

    val has_src_where : t -> f:(elt -> bool) -> bool
    (** [has_src_where bundle ~f] tests whether [bundle] is a
        source/destination pair whose source satisfies [f]. *)

    val has_dst_where : t -> f:(elt -> bool) -> bool
    (** [has_dst_where bundle ~f] tests whether [bundle] is a
        source/destination pair whose source satisfies [f]. *)

    val has_immediate_heap_symbol
      :  t
      -> symbol_table:Symbol.Table.t
      -> bool
    (** [has_immediate_heap_symbol bundle ~symbol_table] returns
       [true] if any operand in [bundle] matches
       [is_immediate_heap_symbol ~syms]. *)

    val is_single_jump_symbol_where
      :  t
      -> f:(Symbol.t -> bool)
      -> bool
    (** [is_single_jump_symbol_where operands ~f] tests whether
        [operands] contains a single operand that matches
        [is_jump_symbol_where ~f]. *)
  end

  (** [Inherit_predicates] generates a [S_predicates] by inheriting it
      from an optional component.  Each predicate returns false when the
      component doesn't exist. *)
  module Inherit_predicates
    : functor (P : S_predicates)
      -> functor (I : Utils.Inherit.S_partial with type c := P.t)
        -> S_predicates with type t := I.t
  ;;

  (** [Flag] is an enumeration of various operand-bundle
     observations. *)
  module Flag : sig
    type t =
      [ `On_src  of Flag.t
      | `On_dst  of Flag.t
      | `On_fst  of Flag.t
      | `On_snd  of Flag.t
      | `On_self of Flag.t
      ]
    [@@deriving sexp, enumerate]
    ;;

    include Flag_enum.S with type t := t
  end

  (** [S_properties] is the signature of any module that can access
     properties (including predicates) of an abstract operand
     bundle. *)
  module type S_properties = sig
    (** [t] is the type we're querying. *)
    type t

    (** Anything that can access properties can also access predicates. *)
    include S_predicates with type t := t

    (** [errors bundle] retrieves a list of error messages corresponding
        to erroneous operands (if any) in [bundle]. *)
    val errors : t -> Error.t list

    (** [flags x symbol_table] gets the statement flags for [x] given
        symbol table [symbol_table]. *)
    val flags : t -> Symbol.Table.t -> Flag.Set.t
  end

  (** [Inherit_properties] generates a [S_properties] by inheriting it
      from a component. *)
  module Inherit_properties
    : functor (P : S_properties)
      -> functor (I : Utils.Inherit.S with type c := P.t)
        -> S_properties with type t := I.t
  ;;

  (** This module contains [S_properties] directly. *)
  include S_properties with type t := t
  (** Operand bundles are traversable containers. *)
  include Travesty.Traversable.S0_container with type elt := elt and type t := t
  include Node.S with type t := t and module Flag := Flag
  include Pretty_printer.S with type t := t
end
