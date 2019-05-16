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

open Stdio

(** An example 'valid' Litmus7 output with no postcondition. *)
let test_output_valid_no_pc : string =
  {|
%%%%%%%%%%%%%%%%%%%
% Results for foo %
%%%%%%%%%%%%%%%%%%%
X86 foo

{A=0; B=0; C=0; D=0; x=0; y=0;}

 P0          | P1          | P2           | P3           ;
 MOVL [x],$1 | MOVL [y],$1 | MOVL EAX,[x] | MOVL EAX,[y] ;
 MFENCE      | MFENCE      | MOVL [C],EAX | MOVL [A],EAX ;
             |             | MOVL EAX,[y] | MOVL EAX,[x] ;
             |             | MOVL [D],EAX | MOVL [B],EAX ;

locations [A; B; C; D; x; y;]
forall (true)
Generated assembler
	##START _litmus_P0
	movl	$1, -4(%rdi,%rax,4)
	mfence
	##START _litmus_P1
	movl	$1, -4(%rdi,%rax,4)
	mfence
	##START _litmus_P2
	movl	-4(%rbx,%rcx,4), %eax
	movl	%eax, -4(%r9,%rcx,4)
	movl	-4(%rdx,%rcx,4), %eax
	movl	%eax, -4(%r10,%rcx,4)
	##START _litmus_P3
	movl	-4(%rdx,%rcx,4), %eax
	movl	%eax, -4(%r9,%rcx,4)
	movl	-4(%rbx,%rcx,4), %eax
	movl	%eax, -4(%r10,%rcx,4)

Test foo Required
Histogram (15 states)
185071:>A=0; B=0; C=0; D=0; x=1; y=1;
1099  :>A=1; B=0; C=0; D=0; x=1; y=1;
52588 :>A=0; B=1; C=0; D=0; x=1; y=1;
103707:>A=1; B=1; C=0; D=0; x=1; y=1;
614   :>A=0; B=0; C=1; D=0; x=1; y=1;
78685 :>A=0; B=1; C=1; D=0; x=1; y=1;
22551 :>A=1; B=1; C=1; D=0; x=1; y=1;
52420 :>A=0; B=0; C=0; D=1; x=1; y=1;
68912 :>A=1; B=0; C=0; D=1; x=1; y=1;
8     :>A=0; B=1; C=0; D=1; x=1; y=1;
58836 :>A=1; B=1; C=0; D=1; x=1; y=1;
104158:>A=0; B=0; C=1; D=1; x=1; y=1;
32800 :>A=1; B=0; C=1; D=1; x=1; y=1;
60014 :>A=0; B=1; C=1; D=1; x=1; y=1;
178537:>A=1; B=1; C=1; D=1; x=1; y=1;
Ok

Witnesses
Positive: 1000000, Negative: 0
Condition forall (true) is validated
Hash=7bfcb06facbe75eff0dc2e656dd34eb4
Observation foo Always 1000000 0
Time foo 0.33
|}

let print_output_from_string (s : string) : unit =
  print_s
    [%sexp
      (Act_sim_litmus.Reader.read_output_from_string s : Act_sim.Output.t)]

let%expect_test "valid output without postcondition parses correctly" =
  print_output_from_string test_output_valid_no_pc ;
  [%expect
    {|
    (Success
     ((states
       (((A 1) (B 1) (C 1) (D 1) (x 1) (y 1))
        ((A 0) (B 1) (C 1) (D 1) (x 1) (y 1))
        ((A 1) (B 0) (C 1) (D 1) (x 1) (y 1))
        ((A 0) (B 0) (C 1) (D 1) (x 1) (y 1))
        ((A 1) (B 1) (C 0) (D 1) (x 1) (y 1))
        ((A 0) (B 1) (C 0) (D 1) (x 1) (y 1))
        ((A 1) (B 0) (C 0) (D 1) (x 1) (y 1))
        ((A 0) (B 0) (C 0) (D 1) (x 1) (y 1))
        ((A 1) (B 1) (C 1) (D 0) (x 1) (y 1))
        ((A 0) (B 1) (C 1) (D 0) (x 1) (y 1))
        ((A 0) (B 0) (C 1) (D 0) (x 1) (y 1))
        ((A 1) (B 1) (C 0) (D 0) (x 1) (y 1))
        ((A 0) (B 1) (C 0) (D 0) (x 1) (y 1))
        ((A 1) (B 0) (C 0) (D 0) (x 1) (y 1))
        ((A 0) (B 0) (C 0) (D 0) (x 1) (y 1))))
      (is_undefined false))) |}]
