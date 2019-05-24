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

(** Language abstraction layer: Instructions

    This module contains the interface act languages must implement for
    instruction analysis, [Basic] and [Basic_with_modules]; the expanded
    interface the rest of act gets, [S]; and a functor from one to the
    other, [Make]. *)

open Core_kernel

(** [Basic] is the interface act languages must implement for instruction
    analysis. *)
module type Basic = sig
  (** Type of instructions. *)
  type t [@@deriving sexp, eq]

  (** [con] is the type of constants. *)
  type con

  (** The type of locations inside instructions. *)
  module Loc : Equal.S

  (** The type of concrete symbols. *)
  module Sym : Equal.S

  (** Languages must supply a pretty-printer for their instructions. *)
  include Pretty_printer.S with type t := t

  val pp_operands : Format.formatter -> t -> unit
  (** [pp_operands f ins] pretty-prints only the instruction operands. *)

  include
    Act_abstract.Abstractable.S
    with type t := t
     and module Abs := Act_abstract.Instruction

  (** They must allow traversal over symbols... *)
  module On_symbols :
    Travesty.Traversable.S0 with module Elt = Sym and type t := t

  (** ...and over locations. *)
  module On_locations :
    Travesty.Traversable.S0 with module Elt = Loc and type t := t

  val jump : string -> t
  (** [jump sym] builds an unconditional jump to symbol [sym]. *)

  val immediate_move : src:con -> dst:Loc.t -> t Or_error.t
  (** [immediate_move ~src ~dst] tries to build a move of immediate value
      [src] into location [dst]. It can fail if the resulting movement is
      inexpressible in this language. *)

  val location_move : src:Loc.t -> dst:Loc.t -> t Or_error.t
  (** [location_move ~src ~dst] tries to build a move from location [src] to
      location [dst]. It can fail if the resulting movement is inexpressible
      in this language. *)

  val as_move_immediate : t -> [< `Src | `Dst] -> con option
  (** [as_move_immediate ins pos] tries to interpret [ins] as a move
      instruction with an immediate value (symbol, integer, etc) in position
      [pos], and, if it is, returns that value. *)

  val as_move_symbol : t -> [< `Src | `Dst] -> Sym.t option
  (** [as_move_symbol ins] tries to interpret [ins] as a move instruction
      with an immediate symbol in position [pos], and, if it is, returns
      that symbol. *)

  val as_move_location : t -> [< `Src | `Dst] -> Loc.t option
  (** [as_move_location ins] tries to interpret [ins] as a move instruction
      with a location in position [pos], and, if it is, returns that
      location. *)

  val abs_operands : t -> Act_abstract.Operand.Bundle.t
  (** [abs_operands ins] gets the abstracted operands of instruction [ins]. *)
end

(** [R_map] is the interface of a map-like structure that represents
    redirections from symbols to other symbols.

    Implementations of [S] contain implementations of [R_map]. *)
module type R_map = sig
  (** [t] is the opaque type of a redirect map. *)
  type t

  (** [sym] is the type of symbols. *)
  type sym [@@deriving sexp, eq]

  (** [r_dest] is the type of destination results used in [R_map]. *)
  type r_dest =
    | Identity  (** The map redirects this symbol to itself *)
    | MapsTo of sym  (** The map redirects this symbol here *)
  [@@deriving sexp, eq]

  module Set : Set.S with type Elt.t = sym

  val make : Set.t -> t
  (** [make s] creates a redirect map with the initial source set [s]. Each
      symbol is registered as mapping to itself. *)

  val redirect : src:sym -> dst:sym -> t -> t Or_error.t
  (** [redirect ~src ~dst rmap] marks [src] as having redirected to [dst] in
      [rmap]. It is safe for [src] and [dst] to be equal: in such cases we
      register that a redirection happened, but otherwise don't do anything.

      [redirect] fails if [dst] is a registered source other than [src];
      this is to prevent cycles. *)

  val dest_of : t -> sym -> r_dest option
  (** [dest_of rmap src] gives the final destination of [src] as an
      [r_dest]. *)

  val all_dests : t -> Set.t
  (** [all_dests rmap] collects all of the destination symbols in [rmap].
      This is a useful approximation as to which symbols are heap
      references. *)

  val sources_of : t -> sym -> sym list
  (** [sources_of rmap dst] gives all of the symbols that map to [dst] in
      [rmap]. *)
end

(** [Basic_with_modules] extends [Basic] with the fully expanded language
    abstraction layer modules on which [Make] depends. *)
module type Basic_with_modules = sig
  module Constant : Constant.S

  module Location : Location.S

  module Symbol : Symbol.S

  include
    Basic
    with type con := Constant.t
     and module Loc := Location
     and module Sym := Symbol
end

(** [S] is an expanded interface onto an act language's instruction
    analysis. *)
module type S = sig
  include Basic_with_modules

  (** We can query abstract properties directly on the concrete instruction
      type. *)
  include Act_abstract.Instruction.S_properties with type t := t

  (** We can query abstract operand bundle properties on a concrete
      instruction type, routing through [abs_operands]. *)
  module On_operands :
    Act_abstract.Operand.Bundle.S_properties with type t := t
end
