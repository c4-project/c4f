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

(** Signatures for the {{!Explainer} Explainer} module. *)

open Base

(** [Basic] collects the input required to generate explainers. This should
    be a subset of {{!Runner.Basic} Runner.Basic}. *)
module type Basic = sig
  (** [Src_lang] is the language under explanation. *)
  module Src_lang : Act_language.Definition.S

  module Sanitiser_hook :
    Act_sanitiser.Hook_intf.S with module Lang = Src_lang

  module Program :
    Plumbing.Loadable_types.S with type t = Src_lang.Program.t
end

module type S = sig
  type config

  (** [Lang] is the language definition this explainer is targeting. *)
  module Lang : Act_language.Definition.S

  (** [Loc_explanation] provides explanations for locations. *)
  module Loc_explanation :
    Explanation_intf.S
    with type elt := Lang.Location.t
     and type context := Act_abstract.Symbol.Table.t
     and module Abs := Act_abstract.Location

  (** [Ops_explanation] provides explanations for instruction operand
      bundles. *)
  module Ops_explanation : sig
    include
      Explanation_intf.S
      with type elt := Lang.Instruction.t
       and type context := Act_abstract.Symbol.Table.t
       and module Abs := Act_abstract.Operand.Bundle

    (** As a shorthand, we can query an explanation directly for properties.
        This uses the explanation's stored abstract representation, but
        bypasses its stored flags. *)
    include Act_abstract.Operand.Bundle.S_properties with type t := t
  end

  (** [Ins_explanation] provides explanations for instructions. *)
  module Ins_explanation : sig
    include
      Explanation_intf.S
      with type elt := Lang.Instruction.t
       and type context := Act_abstract.Symbol.Table.t
       and module Abs := Act_abstract.Instruction

    (** As a shorthand, we can query an explanation directly for properties.
        This uses the explanation's stored abstract representation, but
        bypasses its stored flags. *)
    include Act_abstract.Instruction.S_properties with type t := t
  end

  (** [Stm_explanation] provides explanations for [statement]s, given symbol
      tables as context. *)
  module Stm_explanation : sig
    include
      Explanation_intf.S
      with type elt := Lang.Statement.t
       and type context := Act_abstract.Symbol.Table.t
       and module Abs := Act_abstract.Statement

    (** As a shorthand, we can query an explanation directly for properties.
        This uses the explanation's stored abstract representation, but
        bypasses its stored flags. *)
    include Act_abstract.Statement.S_properties with type t := t
  end

  (** Type of whole-program explanations. *)
  type t =
    { statements: Stm_explanation.t list
    ; symbol_table: Act_abstract.Symbol.Table.t }

  (** We can [pp] explanations. *)
  include Pretty_printer.S with type t := t

  val print_symbol_table : ?oc:Stdio.Out_channel.t -> t -> unit
  (** [print_symbol_table ?oc exp] prints [exp]'s symbol table to stdout, or
      [oc] if given. *)

  val pp_as_assembly : Base.Formatter.t -> t -> unit
  (** [pp_as_assembly f exp] prints [exp] in a smaller summary format that
      (ideally) parses as valid assembly. *)

  val explain : Lang.Program.t -> Act_abstract.Symbol.Table.t -> t
  (** [explain prog symbol_table] compiles a [t] for program [prog], whose
      symbol table is [symbol_table]. *)

  (** [Filter] is the explainer packaged up as an assembly job runner, ie a
      filter accepting an assembly job and outputting a standard job output. *)
  module Filter : Runner_intf.S with type cfg = config
end
