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

open Base

(** [Basic] is the interface act languages must implement for program
    analysis. *)
module type Basic = sig
  (** Type of programs. *)
  type t [@@deriving sexp, eq]

  (** Type of statements inside programs. *)
  type stm

  (** Languages must supply a pretty-printer for their programs. *)
  include Pretty_printer.S with type t := t

  (** They must allow traversal over statement lists (as a block).

      For individual statements, use [On_statements] in {{!S} S}. *)
  module On_listings :
    Travesty.Traversable.S0 with type Elt.t = stm list and type t := t

  val name : t -> string option
  (** [name prog] gets the declared name of [prog], if it has one.

      In high-level languages, for instance, [name] will be the function
      name. *)

  val split :
    t -> f:(stm list -> stm list list Or_error.t) -> t list Or_error.t
  (** [split prog ~f] uses [f] to split [prog]'s statement list into
      sub-lists, then expands each sub-list into a new program. *)

  val dump_as_asm_template : t -> oc:Stdio.Out_channel.t -> unit Or_error.t
  (** [dump_as_asm_template t ~oc] outputs a GCC assembly template
      representation of this program onto [oc]. It can fail, for example if
      the language doesn't support such dumping. *)
end

(** [Basic_with_modules] extends [Basic] with the fully expanded language
    abstraction layer modules on which [Make] depends. *)
module type Basic_with_modules = sig
  module Statement : Statement.S

  include Basic with type stm := Statement.t
end

(** [S] is an expanded interface onto an act language's program analysis. *)
module type S = sig
  include Basic_with_modules

  (** Traversal over every statement in the program (transitive across
      statements of listings). *)
  module On_statements : sig
    include
      Travesty.Traversable.S0 with type Elt.t = Statement.t and type t := t

    include
      Travesty.Filter_mappable.S0 with type elt := Elt.t and type t := t
  end

  val listing : t -> Statement.t list
  (** [listing program] is shorthand for [On_statements.to_list program]. *)

  (** Traversal over every symbol in the program (transitive across symbols
      of statements). *)
  module On_symbols :
    Travesty.Traversable.S0
    with type Elt.t = Statement.Instruction.Symbol.t
     and type t := t

  val symbols :
       t
    -> known_heap_symbols:Act_abstract.Symbol.Set.t
    -> Act_abstract.Symbol.Table.t
  (** [symbols ~known_heap_symbols prog] calculates the symbol table for
      [prog]. It uses [known_heap_symbols] to help work out corner cases in
      heap symbol detection (for example, when heap references are being
      used as immediate values). *)

  val split_on_boundaries : t -> t list Or_error.t
  (** [split_on_boundaries prog] attempts to split [prog] based on program
      boundaries. *)
end
