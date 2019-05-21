(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   (parts (c) 2010-2018 Institut National de Recherche en Informatique et en
   Automatique, Jade Alglave, and Luc Maranget)

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
   USE OR OTHER DEALINGS IN THE SOFTWARE.

   This file derives in part from the Herd7 project
   (https://github.com/herd/herdtools7); its original attribution and
   copyright notice follow. *)

(* the diy toolsuite

   Jade Alglave, University College London, UK.

   Luc Maranget, INRIA Paris-Rocquencourt, France.

   Copyright 2010-present Institut National de Recherche en Informatique et
   en Automatique and the authors. All rights reserved.

   This software is governed by the CeCILL-B license under French law and by
   the rules of distribution of free software. You can use, and/ or
   redistribute the software under the terms of the CeCILL-B license as
   circulated by CEA, CNRS and INRIA at the following URL
   "http://www.cecill.info". We also give a copy in LICENSE.txt. *)

(** Generic, low-level abstract syntax tree for AT&T and Intel x86 *)

open Act_common
open Core_kernel
open Act_utils

(** [Disp] concerns displacements. *)
module Disp : sig
  type t = Symbolic of string | Numeric of int
  [@@deriving sexp, equal, quickcheck, compare]

  (** [On_symbols] permits enumerating and folding over symbols inside a
      displacement. *)
  module On_symbols :
    Travesty.Traversable.S0 with type t := t and type Elt.t = string
end

(** [Index] concerns index-scale pairs. *)
module Index : sig
  type t = Unscaled of Reg.t | Scaled of Reg.t * int
  [@@deriving sexp, eq, compare]

  (** [On_registers] permits enumerating and folding over registers inside a
      displacement. *)
  module On_registers :
    Travesty.Traversable.S0 with type t := t and type Elt.t = Reg.t

  include Quickcheck.S with type t := t
end

module Indirect : sig
  (** [t] is the opaque type of indirect memory accesses. *)
  type t [@@deriving equal]

  val make :
    ?seg:Reg.t -> ?disp:Disp.t -> ?base:Reg.t -> ?index:Index.t -> unit -> t
  (** [make ?seg ?disp ?base ?index ()] makes an [Indirect] with the given
      fields (if present). *)

  val base : t -> Reg.t option
  (** [base] gets the indirect base, if any. *)

  val seg : t -> Reg.t option
  (** [seg] gets the indirect segment, if any. *)

  val disp : t -> Disp.t option
  (** [disp] gets the indirect displacement, if any. *)

  val index : t -> Index.t option
  (** [index] gets the indirect index, if any. *)

  (** [On_registers] permits enumerating and folding over registers inside a
      memory access. *)
  module On_registers :
    Travesty.Traversable.S0 with type t := t and type Elt.t = Reg.t

  (** [On_symbols] permits enumerating and folding over symbols inside a
      memory access. *)
  module On_symbols :
    Travesty.Traversable.S0 with type t := t and type Elt.t = string

  include Quickcheck.S with type t := t
end

(** A syntactic memory locations.

    These are usually indirect seg/disp/base/index stanzas (see
    {{!Indirect} Indirect}), or registers (see {{!Reg} Reg}).

    These can also be template interpolation tokens. This is to support
    assembly syntaxes that represent the template string of a GCC-style
    inline assembly directive; x86 assembly, technically speaking, has no
    concept. *)
module Location : sig
  (** Type of locations in an AST. *)
  type t =
    | Indirect of Indirect.t  (** An indirect location. *)
    | Reg of Reg.t  (** A direct register location. *)
    | Template_token of string
        (** An interpolation from some form of assembly template, for
            example GCC's C {i asm} extension. *)
  [@@deriving sexp, equal, compare]

  (** [On_registers] permits enumerating and folding over registers inside a
      location. *)
  module On_registers :
    Travesty.Traversable.S0 with type t := t and type Elt.t = Reg.t

  (** [On_symbols] permits enumerating and folding over symbols inside a
      location. *)
  module On_symbols :
    Travesty.Traversable.S0 with type t := t and type Elt.t = string

  include Quickcheck.S with type t := t
end

module Bop : sig
  type t = Plus | Minus [@@deriving sexp]

  include Enum.Extension_table with type t := t

  include Quickcheck.S with type t := t
end

module Operand : sig
  type t =
    | Location of Location.t
    | Immediate of Disp.t
    | String of string
    | Typ of string  (** Type annotation *)
    | Bop of t * Bop.t * t
  [@@deriving sexp, equal, compare]

  val location : Location.t -> t

  val immediate : Disp.t -> t

  val string : string -> t

  val typ : string -> t

  val bop : t -> Bop.t -> t -> t

  include Quickcheck.S with type t := t

  (** [On_locations] permits enumerating and folding over locations inside
      an operand. *)
  module On_locations :
    Travesty.Traversable.S0 with type t := t and type Elt.t = Location.t

  (** [On_symbols] permits enumerating and folding over symbols inside an
      operand. *)
  module On_symbols :
    Travesty.Traversable.S0 with type t := t and type Elt.t = string
end

type prefix = PreLock [@@deriving sexp]

(** [Instruction] contains the instruction type and related operations. *)
module Instruction : sig
  (** [t] is the type of instructions (and instruction-like things, such as
      directives). *)
  type t =
    {prefix: prefix option; opcode: Opcode.t; operands: Operand.t list}
  [@@deriving sexp, equal, make]

  (** [On_locations] permits enumerating and folding over locations inside
      an instruction. *)
  module On_locations :
    Travesty.Traversable.S0 with type t := t and type Elt.t = Location.t

  (** [On_symbols] permits enumerating and folding over symbols inside an
      instruction. *)
  module On_symbols :
    Travesty.Traversable.S0 with type t := t and type Elt.t = string
end

module Statement : sig
  type t = Instruction of Instruction.t | Label of string | Nop
  [@@deriving sexp, equal]

  val instruction : Instruction.t -> t
  (** [instruction] creates an instruction statement. *)

  (** [On_instructions] permits enumerating and folding over instructions
      inside a statement. *)
  module On_instructions :
    Travesty.Traversable.S0 with type t := t and type Elt.t = Instruction.t

  (** [On_symbols] permits enumerating and folding over symbols inside an
      operand. *)
  module On_symbols :
    Travesty.Traversable.S0 with type t := t and type Elt.t = string
end

(** Opaque type of dialect-tagged abstract syntax trees. *)
type t [@@deriving sexp, equal]

val make : ?program:Statement.t list -> dialect:Id.t -> unit -> t
(** [make ?program ~dialect ()] makes an AST with program [program]
    (default: empty), tagged with dialect [dialect]. *)

val dialect : t -> Id.t
(** [dialect ast] gets the dialect with which [ast] is tagged. *)

val program : t -> Statement.t list
(** [program ast] gets the list of statements inside [ast]. *)

(** Traversing over the statement list in a [t] *)
module On_listings :
  Travesty.Traversable.S0 with type t := t and type Elt.t = Statement.t list

(** Traversing over all statements in a [t] *)
module On_statements :
  Travesty.Traversable.S0 with type t := t and type Elt.t = Statement.t
