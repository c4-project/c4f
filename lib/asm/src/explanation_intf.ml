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

(** [Basic] is the signature that must be implemented by any module going
    into [Make]. *)
module type Basic = sig
  type elt
  (** Type of the concrete item being explained. *)

  type context
  (** Type of context that must be supplied with an item ([elt]) to compute
      an explanation. *)

  type details
  (** Type of additional information included in the explanation (for
      example, recursive explanations of child components). *)

  include Act_abstract.Abstractable_types.S with type t := elt
  (** Items of type [elt] must be abstractable. *)

  include Pretty_printer.S with type t := elt
  (** Items of type [elt] must also be printable. *)

  module Flag : Act_abstract.Flag_enum.S
  (** [Flag] is the module containing detail flags that may be raised on
      [elt]. *)

  val abs_flags : elt -> context -> Set.M(Flag).t
  (** [abs_flags elt context] computes the set of detail flags that apply to
      [elt] given the context [context]. *)

  val make_details : elt -> context -> details
  (** [make_details elt context] computes any additional details to attach to
      the explanation. *)

  val pp_details : Formatter.t -> details -> unit
  (** [pp_details f details] pretty-prints [details] on [f]. *)
end

(** [S] modules contain an abstract data type [t] that encapsulates a
    concrete but [Abstractable] type [elt] alongside a pre-computed summary
    of its abstract analysis with respect to a given context of type
    [context]. *)
module type S = sig
  type t
  (** Opaque type of the explanation itself. *)

  type elt
  (** Type of the concrete item being explained. *)

  type context
  (** Type of context that must be supplied with an item ([elt]) to compute
      an explanation. *)

  type details
  (** Type of additional information included in the explanation (for
      example, recursive explanations of child components). *)

  include Act_abstract.Abstractable_types.S with type t := t
  (** Each [t] inherits the abstract type projection of its underlying [exp]. *)

  include Pretty_printer.S with type t := t
  (** Explanations can be pretty-printed. *)

  module Flag : Act_abstract.Flag_enum.S
  (** [Flag] is the module containing detail flags that may be raised on the
      explanation's original element. *)

  val original : t -> elt
  (** [original exp] gets the original element of an explanation [exp]. *)

  val details : t -> details
  (** [details exp] gets the additional details of an explanation [exp]. *)

  val abs_flags : t -> Set.M(Flag).t
  (** [abs_flags exp] gets detailed analysis of the element inside [exp] in
      the form of the abstract representation's flag set. *)

  val make : context:context -> original:elt -> t
  (** [make ~context ~original] creates a [t] over [original], using
      [context] to get more detailed information than would normally be
      available through [abs_type] alone. *)
end

module type Basic_ins = sig
  module Location : Act_language.Location_types.S

  module Loc_expl :
    S
      with type elt := Location.t
       and type context := Act_abstract.Symbol.Table.t
       and module Abs := Act_abstract.Location

  module Instruction :
    Act_language.Instruction_types.S with module Location = Location

  module Ops_expl :
    S
      with type elt := Instruction.t
       and type context := Act_abstract.Symbol.Table.t
       and module Abs := Act_abstract.Operand.Bundle
end

module type Basic_stm = sig
  module Instruction : Act_language.Instruction_types.S

  module Ins_expl :
    S
      with type elt := Instruction.t
       and type context := Act_abstract.Symbol.Table.t
       and module Abs := Act_abstract.Instruction

  module Statement :
    Act_language.Statement_types.S with module Instruction = Instruction
end
