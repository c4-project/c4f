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

open Act_common
open Base
open Travesty_containers
include Language_definition_intf

module Common = struct
  let make_heap_loc l =
    Ast.(Location.Indirect (Indirect.make ~disp:(Disp.Symbolic l) ()))

  let register_abs_type : Ast.Reg.t -> Act_abstract.Location.Register.t =
    function
    (* Technically, [E]SP is the 'stack pointer' on x86. However, stack
       offsets generally descend from [E]BP, so we map it to the 'abstract'
       stack pointer. *)
    | `BP | `EBP | `SP | `ESP ->
        Stack_pointer
    | #Ast.Reg.gp as reg ->
        General (Ast.Reg.to_string reg)
    | #Ast.Reg.sp | #Ast.Reg.flag ->
        Unknown

  let disp_abs_type : Ast.Disp.t -> Act_abstract.Location.Address.t =
    function
    | Ast.Disp.Numeric k ->
        Act_abstract.Location.Address.Int k
    | Ast.Disp.Symbolic k ->
        Act_abstract.Location.Address.Symbol k

  let indirect_abs_type (i : Ast.Indirect.t) =
    let open Act_abstract.Location in
    let open Ast.Indirect in
    match (seg i, disp i, base i, index i) with
    | None, disp, Some b, None ->
        let reg = register_abs_type b in
        let offset =
          Option.value_map disp ~f:disp_abs_type
            ~default:(Act_abstract.Location.Address.Int 0)
        in
        Act_abstract.Location.Register_indirect {reg; offset}
    (* This may be over-optimistic. *)
    | None, Some d, None, None ->
        Act_abstract.Location.Heap (disp_abs_type d)
    | _, _, _, _ ->
        Unknown

  module Abstract_location = Act_abstract.Abstractable.Make (struct
    type nonrec t = Ast.Location.t

    module Abs = Act_abstract.Location

    let abstract = function
      | Ast.Location.Reg reg ->
          Abs.Register_direct (register_abs_type reg)
      | Indirect i ->
          indirect_abs_type i
      | Template_token _ ->
          Unknown
  end)

  module Symbol = struct
    include String

    let require_of_string = Or_error.return

    let abstract = Fn.id

    let abstract_demangle str =
      (* These are the types of manglings we've seen in practice: *)
      List.filter_opt
        [ Some str (* GNU/Linux ELF *)
        ; String.chop_prefix ~prefix:"_" str
          (* Darwin Mach-O *) ]

    module On_strings = Travesty.Traversable.Fix_elt (Singleton) (String)
  end

  module Statement = struct
    type t = Ast.Statement.t [@@deriving sexp, equal]

    let empty () = Ast.Statement.Nop

    let label s = Ast.Statement.Label s

    let instruction = Ast.Statement.instruction

    (* Statement abstraction is dialect-dependent, so we don't define it
       here. *)

    module On_symbols = Ast.Statement.On_symbols
    module On_instructions = Ast.Statement.On_instructions
  end

  module Location = struct
    type t = Ast.Location.t [@@deriving sexp, eq]

    let make_heap_loc = make_heap_loc

    module Sym = Symbol
    include Abstract_location
    module On_symbols = Ast.Location.On_symbols
  end

  module Program = struct
    type t = Ast.t [@@deriving sexp, equal]

    let name = Fn.const None

    module On_listings = Ast.On_listings

    let split prog ~f =
      let open Or_error.Let_syntax in
      let body = Ast.program prog in
      let%map bodies' = f body in
      List.map bodies' ~f:(fun body' ->
          On_listings.map prog ~f:(fun _ -> body') )
  end

  module Basic = struct
    let name = "X86"

    module Symbol = Symbol
  end
end

module Make_basic (T : Dialect.S) (P : Pp.Printer) = struct
  include Common.Basic

  let pp_comment = P.pp_comment

  module Location = struct
    include Common.Location

    let pp = P.pp_location
  end

  module Instruction = Language_instruction.Make (struct
    module Dialect = T
    module Pretty = P
    module Symbol = Symbol
    module Location = Location
  end)

  module Statement = struct
    include Common.Statement

    let pp = P.pp_statement

    include Act_abstract.Abstractable.Make (struct
      type nonrec t = Ast.Statement.t

      module Abs = Act_abstract.Statement
      open Abs

      let abstract = function
        | Ast.Statement.Instruction {opcode= Opcode.Directive s; _} ->
            Abs.Directive s
        | Instruction i ->
            Instruction (Instruction.abstract i)
        | Label l ->
            Label l
        | Nop ->
            Blank
    end)
  end

  module Program = struct
    include Common.Program

    let pp = P.pp

    let dump_as_asm_template (asm : Ast.t) ~(oc : Stdio.Out_channel.t) :
        unit Or_error.t =
      (* TODO(@MattWindsor91): implement *)
      ignore (asm : Ast.t) ;
      ignore (oc : Stdio.Out_channel.t) ;
      Or_error.unimplemented "TODO: x86 asm template dumping"
  end

  module Constant = struct
    (* TODO: this is too weak *)
    include Ast.Operand

    let pp = P.pp_operand

    let of_int (k : int) = Ast.Operand.Immediate (Ast.Disp.Numeric k)
  end
end

module Make (T : Dialect.S) (P : Pp.Printer) : S = struct
  module Dialect = T
  module Basic = Make_basic (T) (P)
  include Act_language.Definition.Make (Basic)

  let make_jump_operand = Basic.Instruction.make_jump_operand
end

module Att = Make (Dialect.Att) (Pp.Att)
module Intel = Make (Dialect.Intel) (Pp.Intel)
module Herd7 = Make (Dialect.Herd7) (Pp.Herd7)

let dialect_table : (Id.t, (module S)) List.Assoc.t Lazy.t =
  lazy
    [ (Id.of_string "att", (module Att))
    ; (Id.of_string "intel", (module Intel))
    ; (Id.of_string "herd7", (module Herd7)) ]

let of_dialect : Id.t -> (module S) Or_error.t =
  Staged.unstage
    (Dialect.find_by_id dialect_table
       ~context:"finding a language definition")
