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
open Act_c.Asm_stub

let%test_module "pretty printing" =
  ( module struct
    let test : t -> unit = Fmt.pr "@[%a@]@." pp

    let c_id (s : string) : Act_c_lang.Ast.Expr.t =
      Act_c_lang.Ast.Expr.Identifier (Act_common.C_id.of_string s)

    let%expect_test "rdtsc example from GCC documentation" =
      test
        (make
           ~template:["rdtsc"; "shl $32, %%rdx"; "or %%rdx, %0"]
           ~output_operands:[Operand.make ~constr:"=a" ~rhs:(c_id "msr") ()]
           ~clobbers:["rdx"] ()) ;
      [%expect
        {|
asm volatile
  ("rdtsc\n\t"
   "shl $32, %%rdx\n\t"
   "or %%rdx, %0"
   : "=a" (msr)
   :
   : "rdx" );
|}]
  end )
