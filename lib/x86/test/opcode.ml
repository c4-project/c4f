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
open Stdio
open Act_x86.Opcode

let%test_module "Basic" = (module struct
  open Basic
  let%expect_test "table accounts for all instructions" =
    Fmt.pr "@[<v>%a@]@."
      (Fmt.list ~sep:Fmt.sp (fun f opcode ->
           Fmt.pf f "@[<h>%a -> %s@]" Sexp.pp_hum
             [%sexp (opcode : t)]
             (Option.value ~default:"(none)" (to_string opcode)) ))
      all ;
    [%expect
      {|
      Add -> add
      Call -> call
      Cmp -> cmp
      Cmpxchg -> cmpxchg
      Mov -> mov
      Pop -> pop
      Push -> push
      Ret -> ret
      Sub -> sub
      Xchg -> xchg
      Xor -> xor
      Leave -> leave
      Mfence -> mfence
      Nop -> nop |}]
end)

let%test_module "Jump" = (module struct
  open Jump

  let%expect_test "table accounts for all conditions" =
    Fmt.pr "@[<v>%a@]@."
      (Fmt.list ~sep:Fmt.sp (fun f opcode ->
           Fmt.pf f "@[<h>%a -> %s@]" Sexp.pp_hum
             [%sexp (opcode : t)]
             (Option.value ~default:"(none)" (to_string opcode)) ))
      all ;
    [%expect
      {|
      Unconditional -> jmp
      (Conditional Above) -> ja
      (Conditional AboveEqual) -> jae
      (Conditional Below) -> jb
      (Conditional BelowEqual) -> jbe
      (Conditional Carry) -> jc
      (Conditional Equal) -> je
      (Conditional Greater) -> jg
      (Conditional GreaterEqual) -> jge
      (Conditional Less) -> jl
      (Conditional LessEqual) -> jle
      (Conditional Overflow) -> jo
      (Conditional Parity) -> jp
      (Conditional Sign) -> js
      (Conditional Zero) -> jz
      (Conditional (Not Above)) -> jna
      (Conditional (Not AboveEqual)) -> jnae
      (Conditional (Not Below)) -> jnb
      (Conditional (Not BelowEqual)) -> jnbe
      (Conditional (Not Carry)) -> jnc
      (Conditional (Not Equal)) -> jne
      (Conditional (Not Greater)) -> jng
      (Conditional (Not GreaterEqual)) -> jnge
      (Conditional (Not Less)) -> jnl
      (Conditional (Not LessEqual)) -> jnle
      (Conditional (Not Overflow)) -> jno
      (Conditional (Not Parity)) -> jnp
      (Conditional (Not Sign)) -> jns
      (Conditional (Not Zero)) -> jnz
      (Conditional CXZero) -> jcxz
      (Conditional ECXZero) -> jecxz
      (Conditional ParityEven) -> jpe
      (Conditional ParityOdd) -> jpo |}]
end)

let%expect_test "of_string: directive" =
  print_s [%sexp (of_string ".global" : t)] ;
  [%expect {| (Directive global) |}]

let%expect_test "of_string: conditional jump" =
  print_s [%sexp (of_string "jne" : t)] ;
  [%expect {| (Jump (Conditional (Not Equal))) |}]

let%expect_test "of_string: unconditional jump" =
  print_s [%sexp (of_string "JMP" : t)] ;
  [%expect {| (Jump Unconditional) |}]

let%expect_test "of_string: sized opcode" =
  print_s [%sexp (of_string "movl" : t)] ;
  [%expect {| (Sized (Mov Long)) |}]

let%expect_test "of_string: basic opcode" =
  print_s [%sexp (of_string "MOV" : t)] ;
  [%expect {| (Basic Mov) |}]

let%expect_test "of_string: not an opcode" =
  print_s [%sexp (of_string "bananas" : t)] ;
  [%expect {| (Unknown bananas) |}]
