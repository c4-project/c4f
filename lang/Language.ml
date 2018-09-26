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

open Core

type name =
  | X86 of X86Dialect.t
             [@@deriving sexp]

let pp_name ?(show_sublang=true) f = function
  | X86 syn ->
     Format.pp_print_string f "X86";
     if show_sublang
     then Format.fprintf f "@ (%a)"
                         X86Dialect.pp syn

type abs_instruction =
  | AIJump
  | AIMove
  | AINop
  | AICall
  | AIStack
  | AIOther
[@@deriving enum, sexp]

type abs_location =
  | ALStackPointer
  | ALStackOffset of int
  | ALHeap of string
  | ALUnknown

type abs_statement =
  | ASDirective of string
  | ASInstruction of abs_instruction
  | ASBlank
  | ASLabel of string
  | ASOther

module AISet =
  Set.Make(
      struct
        type t = abs_instruction

        let compare x y =
          Int.compare (abs_instruction_to_enum x)
                      (abs_instruction_to_enum y)

        let sexp_of_t = sexp_of_abs_instruction
        let t_of_sexp = abs_instruction_of_sexp
      end
    )

module SymSet = Set.Make(String)

module type BaseS = sig
  val name : name
  val is_program_label : string -> bool
end

module type StatementS = sig
  type t

  include Core.Pretty_printer.S with type t := t

  val fold_map_symbols : f:('a -> string -> 'a * string) ->
                         init:'a ->
                         t ->
                         ('a * t)

  val nop : unit -> t
  val statement_type : t -> abs_statement
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
  include BaseS
  module Statement : StatementS
  module Location : LocationS
  module Constant : ConstantS
end

module type Intf = sig
  include BaseS

  module Statement : sig
    include StatementS

    val map_symbols : f:(string -> string) -> t -> t
    val symbol_set : t -> SymSet.t

    val is_directive : t -> bool
    val is_jump : t -> bool
    val is_label : t -> bool
    val is_nop : t -> bool
    val instruction_type : t -> abs_instruction option
    val instruction_mem : AISet.t -> t -> bool
    val is_program_boundary : t -> bool
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
    let is_program_label = M.is_program_label

    module Statement = struct
      include M.Statement

      let instruction_type stm =
        match statement_type stm with
        | ASInstruction i -> Some i
        | _ -> None

      let instruction_mem set stm =
        Option.exists ~f:(fun it -> AISet.mem set it)
                      (instruction_type stm)

      let is_directive stm =
        match statement_type stm with
        | ASDirective _ -> true
        | _ -> false

      let is_jump stm =
        match instruction_type stm with
        | Some AIJump -> true
        | _ -> false

      let is_label stm =
        match statement_type stm with
        | ASLabel _ -> true
        | _ -> false

      let is_nop stm =
        match statement_type stm with
        | ASBlank -> true
        | ASInstruction AINop -> true
        | _ -> false

      let is_program_boundary stm =
        match statement_type stm with
        | ASLabel l -> is_program_label l
        | _ -> false

      let map_symbols ~f stm =
        snd (fold_map_symbols ~f:(fun _ x -> ((), f x)) ~init:() stm)

      let symbol_set stm =
        fst (fold_map_symbols ~f:(fun set x -> SymSet.add set x, x) ~init:SymSet.empty stm)
    end

    module Location = struct
      include M.Location
    end

    module Constant = struct
      include M.Constant
    end
  end
