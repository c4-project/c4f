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

(** High-level interface to languages supported by act *)

(** [Basic_core] is the signature that act languages must implement in
    regards to miscellaneous facts about the language. *)
module type Basic_core = sig
  val name : string
  (** [name] is the Herd7 name of the language. *)

  val pp_comment :
    pp:(Format.formatter -> 'a -> unit) -> Format.formatter -> 'a -> unit
  (** [pp_comment ~pp f k] prints a line comment whose body is given by
      invoking [pp] on [k]. *)
end

(** [Basic] is the signature that act languages must implement. *)
module type Basic = sig
  include Basic_core

  module Constant : Constant_types.S

  module Symbol : Symbol_types.Basic

  module Location : Location_types.Basic with module Sym := Symbol

  module Instruction :
    Instruction_types.Basic
      with type con = Constant.t
       and module Loc := Location
       and module Sym := Symbol

  module Statement :
    Statement_types.Basic
      with module Ins := Instruction
       and module Sym := Symbol

  module Program : Program_types.Basic with type stm := Statement.t
end

(** [S] is the user-facing interface module for act languages. Usually, you
    can get an [S] by applying the functor [Make] onto a bare-bones language
    [Basic]. *)
module type S = sig
  include Basic_core

  module Constant : Constant_types.S

  module Symbol : Symbol_types.S

  module Location : Location_types.S with module Symbol = Symbol

  module Instruction :
    Instruction_types.S
      with module Constant = Constant
       and module Location = Location
       and module Symbol = Symbol

  module Statement : Statement_types.S with module Instruction = Instruction

  module Program : Program_types.S with module Statement = Statement

  module Element :
    Element_types.S
      with type ins = Instruction.t
       and type loc = Location.t
       and type stm = Statement.t
       and type sym = Symbol.t
end
