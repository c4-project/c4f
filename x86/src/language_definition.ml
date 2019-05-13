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
open Core_kernel
open Travesty_containers
include Language_definition_intf

module Make (T : Dialect.S) (P : Pp.Printer) : S = struct
  include T
  include P

  module Basic = struct
    let name = "X86"

    let pp_comment = P.pp_comment

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

    module Location = struct
      type t = Ast.Location.t [@@deriving sexp, eq]

      let pp = P.pp_location

      module Sym = Symbol

      let make_heap_loc l =
        Ast.(Location.Indirect (Indirect.make ~disp:(Disp.Symbolic l) ()))

      let register_abs_type : Ast.Reg.t -> Abstract.Location.Register.t =
        function
        (* Technically, [E]SP is the 'stack pointer' on x86. However, stack
           offsets generally descend from [E]BP, so we map it to the
           'abstract' stack pointer. *)
        | `BP | `EBP | `SP | `ESP ->
            Stack_pointer
        | #Ast.Reg.gp as reg ->
            General (Ast.Reg.to_string reg)
        | #Ast.Reg.sp | #Ast.Reg.flag ->
            Unknown

      let disp_abs_type : Ast.Disp.t -> Abstract.Location.Address.t =
        function
        | Ast.Disp.Numeric k ->
            Abstract.Location.Address.Int k
        | Ast.Disp.Symbolic k ->
            Abstract.Location.Address.Symbol k

      let indirect_abs_type (i : Ast.Indirect.t) =
        let open Abstract.Location in
        let open Ast.Indirect in
        match (seg i, disp i, base i, index i) with
        | None, disp, Some b, None ->
            let reg = register_abs_type b in
            let offset =
              Option.value_map disp ~f:disp_abs_type
                ~default:(Abstract.Location.Address.Int 0)
            in
            Abstract.Location.Register_indirect {reg; offset}
        (* This may be over-optimistic. *)
        | None, Some d, None, None ->
            Abstract.Location.Heap (disp_abs_type d)
        | _, _, _, _ ->
            Unknown

      include Abstract.Abstractable.Make (struct
        type nonrec t = t

        module Abs = Abstract.Location

        let abstract = function
          | Ast.Location.Reg reg ->
              Abs.Register_direct (register_abs_type reg)
          | Indirect i ->
              indirect_abs_type i
          | Template_token _ ->
              Unknown
      end)

      module On_symbols = Ast.Location.On_symbols
    end

    module Instruction = Language_instruction.Make (struct
      module Dialect = T
      module Pretty = P
      module Symbol = Symbol
      module Location = Location
    end)

    module Statement = struct
      type t = Ast.Statement.t [@@deriving sexp, eq]

      let pp = P.pp_statement

      let empty () = Ast.Statement.Nop

      let label s = Ast.Statement.Label s

      let instruction = Ast.Statement.instruction

      module Abs = Abstract.Statement

      include Abstract.Abstractable.Make (struct
        type nonrec t = t

        module Abs = Abstract.Statement
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

      module On_symbols = Ast.Statement.On_symbols
      module On_instructions = Ast.Statement.On_instructions
    end

    module Program = struct
      type t = Ast.t [@@deriving sexp, eq]

      let pp = P.pp

      let name = Fn.const None

      module On_listings = Ast.On_listings

      let split prog ~f =
        let open Or_error.Let_syntax in
        let body = Ast.program prog in
        let%map bodies' = f body in
        List.map bodies' ~f:(fun body' ->
            On_listings.map prog ~f:(fun _ -> body') )

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

  include Language.Definition.Make (Basic)

  let make_jump_operand = Basic.Instruction.make_jump_operand
end

module Att = Make (Dialect.Att) (Pp.Att)

let%expect_test "is_program_label: positive Mach-O example, AT&T" =
  printf "%b" (Att.Symbol.is_program_label "_P0") ;
  [%expect {| true |}]

let%expect_test "is_program_label: positive ELF example, AT&T" =
  printf "%b" (Att.Symbol.is_program_label "P0") ;
  [%expect {| true |}]

let%expect_test "is_program_label: wrong suffix, Mach-O, AT&T" =
  printf "%b" (Att.Symbol.is_program_label "_P0P") ;
  [%expect {| false |}]

let%expect_test "is_program_label: wrong suffix, ELF, AT&T" =
  printf "%b" (Att.Symbol.is_program_label "P0P") ;
  [%expect {| false |}]

let%expect_test "is_program_label: negative, AT&T" =
  printf "%b" (Att.Symbol.is_program_label "_P-1") ;
  [%expect {| false |}]

let%expect_test "abs_operands: add $-16, %ESP, AT&T" =
  Format.printf "%a@." Abstract.Operand.Bundle.pp
    (Att.Instruction.abs_operands
       (Ast.Instruction.make
          ~opcode:(Opcode.Basic `Add)
          ~operands:
            [ Ast.Operand.Immediate (Ast.Disp.Numeric (-16))
            ; Ast.Operand.Location (Ast.Location.Reg `ESP) ]
          ())) ;
  [%expect {| $-16 -> reg:sp |}]

let%expect_test "abs_operands: nop -> none" =
  Format.printf "%a@." Abstract.Operand.Bundle.pp
    (Att.Instruction.abs_operands
       (Ast.Instruction.make ~opcode:(Opcode.Basic `Nop) ())) ;
  [%expect {| none |}]

let%expect_test "abs_operands: jmp, AT&T style" =
  Format.printf "%a@." Abstract.Operand.Bundle.pp
    (Att.Instruction.abs_operands
       (Ast.Instruction.make
          ~opcode:(Opcode.Jump `Unconditional)
          ~operands:
            [ Ast.Operand.Location
                (Ast.Location.Indirect
                   (Ast.Indirect.make ~disp:(Ast.Disp.Symbolic "L1") ())) ]
          ())) ;
  [%expect {| sym:L1 |}]

let%expect_test "abs_operands: pop $42 -> error" =
  Format.printf "%a@." Abstract.Operand.Bundle.pp
    (Att.Instruction.abs_operands
       (Ast.Instruction.make
          ~opcode:(Opcode.Basic `Pop)
          ~operands:[Ast.Operand.Immediate (Ast.Disp.Numeric 42)]
          ())) ;
  [%expect {| <ERR: Operand type not allowed here> |}]

let%expect_test "abs_operands: nop $42 -> error" =
  Format.printf "%a@." Abstract.Operand.Bundle.pp
    (Att.Instruction.abs_operands
       (Ast.Instruction.make
          ~opcode:(Opcode.Basic `Nop)
          ~operands:[Ast.Operand.Immediate (Ast.Disp.Numeric 42)]
          ())) ;
  [%expect
    {| <ERR: ("Expected zero operands" (got ((Immediate (Numeric 42)))))> |}]

let%expect_test "abs_operands: mov %ESP, %EBP" =
  Format.printf "%a@." Abstract.Operand.Bundle.pp
    (Att.Instruction.abs_operands
       (Ast.Instruction.make
          ~opcode:(Opcode.Basic `Mov)
          ~operands:
            [ Ast.Operand.Location (Ast.Location.Reg `ESP)
            ; Ast.Operand.Location (Ast.Location.Reg `EBP) ]
          ())) ;
  [%expect {| reg:sp -> reg:sp |}]

let%expect_test "abs_operands: movl %ESP, %EBP" =
  Format.printf "%a@." Abstract.Operand.Bundle.pp
    (Att.Instruction.abs_operands
       (Ast.Instruction.make
          ~opcode:(Opcode.Sized (`Mov, Opcode.Size.Long))
          ~operands:
            [ Ast.Operand.Location (Ast.Location.Reg `ESP)
            ; Ast.Operand.Location (Ast.Location.Reg `EBP) ]
          ())) ;
  [%expect {| reg:sp -> reg:sp |}]

module Intel = Make (Dialect.Intel) (Pp.Intel)

let%expect_test "abs_operands: add ESP, -16, Intel" =
  Format.printf "%a@." Abstract.Operand.Bundle.pp
    (Intel.Instruction.abs_operands
       (Ast.Instruction.make
          ~opcode:(Opcode.Basic `Add)
          ~operands:
            [ Ast.Operand.Location (Ast.Location.Reg `ESP)
            ; Ast.Operand.Immediate (Ast.Disp.Numeric (-16)) ]
          ())) ;
  [%expect {| $-16 -> reg:sp |}]

let%expect_test "abs_operands: mov %ESP, $1, AT&T, should be error" =
  Format.printf "%a@." Abstract.Operand.Bundle.pp
    (Att.Instruction.abs_operands
       (Ast.Instruction.make
          ~opcode:(Opcode.Basic `Mov)
          ~operands:
            [ Ast.Operand.Location (Ast.Location.Reg `ESP)
            ; Ast.Operand.Immediate (Ast.Disp.Numeric 1) ]
          ())) ;
  [%expect {| <ERR: Operand types not allowed here> |}]

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
