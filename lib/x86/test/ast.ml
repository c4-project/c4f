(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module X86 = Act_x86

let%expect_test "symbol fold over bop" =
  let ast =
    X86.(
      Ast.(
        Operand.(
          bop
            (bop (symbolic "a") Bop.Plus (symbolic "b"))
            Bop.Minus
            (location
               (Location.Indirect
                  (Indirect.make ~disp:(Disp.Symbolic "c") ()))))))
  in
  let f count sym = (count + 1, String.capitalize sym) in
  let total, ast' = X86.Ast.Operand.On_symbols.fold_map ~f ~init:0 ast in
  Fmt.pr "@[<v>@[<h>Total:@ %d@]@,%a@]@." total Sexp.pp_hum
    [%sexp (ast' : X86.Ast.Operand.t)] ;
  [%expect
    {|
      Total: 3
      (Bop (Bop (Immediate (Symbolic A)) + (Immediate (Symbolic B))) -
       (Location (Indirect ((seg ()) (disp ((Symbolic C))) (base ()) (index ()))))) |}]
