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
open Act_x86
module Io = Act_utils.Io
module Att = Language_definition.Att

let%test_module "is_program_label" =
  ( module struct
    let%expect_test "positive Mach-O example, AT&T" =
      Io.print_bool (Att.Symbol.is_program_label "_P0") ;
      [%expect {| true |}]

    let%expect_test "positive ELF example, AT&T" =
      Io.print_bool (Att.Symbol.is_program_label "P0") ;
      [%expect {| true |}]

    let%expect_test "wrong suffix, Mach-O, AT&T" =
      Io.print_bool (Att.Symbol.is_program_label "_P0P") ;
      [%expect {| false |}]

    let%expect_test "wrong suffix, ELF, AT&T" =
      Io.print_bool (Att.Symbol.is_program_label "P0P") ;
      [%expect {| false |}]

    let%expect_test "negative, AT&T" =
      Io.print_bool (Att.Symbol.is_program_label "_P-1") ;
      [%expect {| false |}]
  end )

let%test_module "abs_operands" =
  ( module struct
    let test : Ast.Instruction.t -> unit =
      Fn.compose
        (Fmt.pr "%a@." Act_abstract.Operand.Bundle.pp)
        Att.Instruction.abs_operands

    let%expect_test "add $-16, %ESP, AT&T" =
      test
        (Ast.Instruction.make
           ~opcode:(Opcode.Basic `Add)
           ~operands:
             [ Ast.Operand.Immediate (Disp.Numeric (-16))
             ; Ast.Operand.Location (Ast.Location.Reg `ESP) ]
           ()) ;
      [%expect {| $-16 -> reg:sp |}]

    let%expect_test "nop -> none" =
      test (Ast.Instruction.make ~opcode:(Opcode.Basic `Nop) ()) ;
      [%expect {| none |}]

    let%expect_test "jmp, AT&T style" =
      test
        (Ast.Instruction.make
           ~opcode:(Opcode.Jump `Unconditional)
           ~operands:
             [ Ast.Operand.Location
                 (Ast.Location.Indirect
                    (Ast.Indirect.make ~disp:(Disp.Symbolic "L1") ())) ]
           ()) ;
      [%expect {| sym:L1 |}]

    let%expect_test "pop $42 -> error" =
      test
        (Ast.Instruction.make
           ~opcode:(Opcode.Basic `Pop)
           ~operands:[Ast.Operand.Immediate (Disp.Numeric 42)]
           ()) ;
      [%expect {| <ERR: Operand type not allowed here> |}]

    let%expect_test "nop $42 -> error" =
      test
        (Ast.Instruction.make
           ~opcode:(Opcode.Basic `Nop)
           ~operands:[Ast.Operand.Immediate (Disp.Numeric 42)]
           ()) ;
      [%expect
        {| <ERR: ("Expected zero operands" (got ((Immediate (Numeric 42)))))> |}]

    let%expect_test "mov %ESP, %EBP" =
      test
        (Ast.Instruction.make
           ~opcode:(Opcode.Basic `Mov)
           ~operands:
             [ Ast.Operand.Location (Ast.Location.Reg `ESP)
             ; Ast.Operand.Location (Ast.Location.Reg `EBP) ]
           ()) ;
      [%expect {| reg:sp -> reg:sp |}]

    let%expect_test "movl %ESP, %EBP" =
      test
        (Ast.Instruction.make
           ~opcode:(Opcode.Sized (`Mov, Opcode.Size.Long))
           ~operands:
             [ Ast.Operand.Location (Ast.Location.Reg `ESP)
             ; Ast.Operand.Location (Ast.Location.Reg `EBP) ]
           ()) ;
      [%expect {| reg:sp -> reg:sp |}]

    let%expect_test "mov %ESP, $1, AT&T, should be error" =
      test
        (Ast.Instruction.make
           ~opcode:(Opcode.Basic `Mov)
           ~operands:
             [ Ast.Operand.Location (Ast.Location.Reg `ESP)
             ; Ast.Operand.Immediate (Disp.Numeric 1) ]
           ()) ;
      [%expect {| <ERR: Operand types not allowed here> |}]
  end )
