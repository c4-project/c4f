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

open Act_x86
module Intel = Language_definition.Intel

let%expect_test "abs_operands: add ESP, -16, Intel" =
  Fmt.pr "%a@." Act_abstract.Operand.Bundle.pp
    (Intel.Instruction.abs_operands
       (Ast.Instruction.make
          ~opcode:(Opcode.Basic `Add)
          ~operands:
            [ Ast.Operand.Location (Ast.Location.Reg `ESP)
            ; Ast.Operand.Immediate (Disp.Numeric (-16)) ]
          ())) ;
  [%expect {| $-16 -> reg:sp |}]

let%test_module "Pretty-printing" =
  ( module struct
    open Act_x86.Pp.Intel

    let%expect_test "pp_comment: Intel" =
      Fmt.pr "%a@." (pp_comment ~pp:Fmt.string) "intel comment" ;
      [%expect {| ; intel comment |}]

    let%expect_test "pp_reg: intel, EAX" =
      Fmt.pr "%a@." pp_reg `EAX ;
      [%expect {| EAX |}]

    let%expect_test "pp_indirect: intel, +ve numeric displacement only" =
      Fmt.pr "%a@." pp_indirect (Indirect.make ~disp:(Disp.Numeric 2001) ()) ;
      [%expect {| [2001] |}]

    let%expect_test "pp_indirect: Intel, +ve disp and base" =
      Fmt.pr "%a@." pp_indirect
        (Indirect.make ~disp:(Disp.Numeric 76) ~base:`EAX ()) ;
      [%expect {| [EAX+76] |}]

    let%expect_test "pp_indirect: Intel, zero disp only" =
      Fmt.pr "%a@." pp_indirect (Indirect.make ~disp:(Disp.Numeric 0) ()) ;
      [%expect {| [0] |}]

    let%expect_test "pp_indirect: Intel, +ve disp and base" =
      Fmt.pr "%a@." pp_indirect
        (Indirect.make ~disp:(Disp.Numeric (-42)) ~base:`ECX ()) ;
      [%expect {| [ECX-42] |}]

    let%expect_test "pp_indirect: Intel, base only" =
      Fmt.pr "%a@." pp_indirect (Indirect.make ~base:`EDX ()) ;
      [%expect {| [EDX] |}]

    let%expect_test "pp_indirect: Intel, zero disp and base" =
      Fmt.pr "%a@." pp_indirect
        (Indirect.make ~disp:(Disp.Numeric 0) ~base:`EDX ()) ;
      [%expect {| [EDX] |}]
  end )
