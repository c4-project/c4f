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

type name =
  | X86 of X86Ast.syntax
             [@@deriving sexp]

let pp_name ?(show_sublang=true) f = function
  | X86 syn ->
     Format.pp_print_string f "X86";
     if show_sublang
     then Format.fprintf f "@ (%a)"
                         X86Ast.pp_syntax syn

type abs_instruction =
  | AbsDirective of string
  | AbsJump of string list
  | AbsMove
  | AbsCall
  | AbsStack
  | AbsOther

module type StatementS = sig
  type t

  include Core.Pretty_printer.S with type t := t

  val map_ids : f:(string -> string) -> t -> t
  val nop : unit -> t
  val instruction_type : t -> abs_instruction option
  val is_nop : t -> bool
  val is_program_boundary : t -> bool
end

module type LocationS = sig
  type t
  include Core.Pretty_printer.S with type t := t
end

module type ConstantS = sig
  type t
  include Core.Pretty_printer.S with type t := t
end

module type S = sig
  val name : name

  module Statement : StatementS
  module Location : LocationS
  module Constant : ConstantS
end

module type Intf = sig
  val name : name

  module Statement : sig
    include StatementS

    val is_directive : t -> bool
  end

  module Location : sig
    include LocationS
  end

  module Constant : sig
    include ConstantS
  end
end

module Make (M : S) =
  struct
    let name = M.name

    module Statement = struct
      include M.Statement

      let is_directive stm =
        match instruction_type stm with
        | Some (AbsDirective _) -> true
        | _ -> false
    end

    module Location = struct
      include M.Location
    end

    module Constant = struct
      include M.Constant
    end
  end
