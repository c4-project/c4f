(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
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

(** An example 'valid' (if peculiar) Litmus7 output with a postcondition. *)
let test_output_valid_pc : string =
  {|
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Results for SB+SC.litmus %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
X86 SB
"Fre PodWR Fre PodWR"

{x=0; y=0;}

 P0          | P1          ;
 MOV [x],$1  | MOV [y],$1  ;
 MOV EAX,[y] | MOV EAX,[x] ;

exists (0:EAX=0 /\ 1:EAX=0)
Generated assembler
Test SB Allowed
Histogram (4 states)
289   *>0:EAX=0; 1:EAX=0;
499805:>0:EAX=1; 1:EAX=0;
499791:>0:EAX=0; 1:EAX=1;
115   :>0:EAX=1; 1:EAX=1;
Ok

Witnesses
Positive: 289, Negative: 999711
Condition exists (0:EAX=0 /\ 1:EAX=0) is validated
Hash=7dbd6b8e6dd4abc2ef3d48b0376fb2e3
Observation SB Sometimes 289 999711
Time SB 0.12
|}

let print_output_from_string (s : string) : unit =
  print_s
    [%sexp
      ( Act_backend_litmus.Reader.load_from_string s
        : Act_state.Observation.t Or_error.t )]

let%expect_test "valid output without postcondition parses correctly" =
  print_output_from_string test_output_valid_no_pc ;
  [%expect
    {|
    (Ok
     ((flags (sat))
      (states
       (((A 0) (B 0) (C 0) (D 0) (x 1) (y 1))
        ((A 0) (B 0) (C 0) (D 1) (x 1) (y 1))
        ((A 0) (B 0) (C 1) (D 0) (x 1) (y 1))
        ((A 0) (B 0) (C 1) (D 1) (x 1) (y 1))
        ((A 0) (B 1) (C 0) (D 0) (x 1) (y 1))
        ((A 0) (B 1) (C 0) (D 1) (x 1) (y 1))
        ((A 0) (B 1) (C 1) (D 0) (x 1) (y 1))
        ((A 0) (B 1) (C 1) (D 1) (x 1) (y 1))
        ((A 1) (B 0) (C 0) (D 0) (x 1) (y 1))
        ((A 1) (B 0) (C 0) (D 1) (x 1) (y 1))
        ((A 1) (B 0) (C 1) (D 1) (x 1) (y 1))
        ((A 1) (B 1) (C 0) (D 0) (x 1) (y 1))
        ((A 1) (B 1) (C 0) (D 1) (x 1) (y 1))
        ((A 1) (B 1) (C 1) (D 0) (x 1) (y 1))
        ((A 1) (B 1) (C 1) (D 1) (x 1) (y 1))))
      (witnesses
       (((A 0) (B 0) (C 0) (D 0) (x 1) (y 1))
        ((A 0) (B 0) (C 0) (D 1) (x 1) (y 1))
        ((A 0) (B 0) (C 1) (D 0) (x 1) (y 1))
        ((A 0) (B 0) (C 1) (D 1) (x 1) (y 1))
        ((A 0) (B 1) (C 0) (D 0) (x 1) (y 1))
        ((A 0) (B 1) (C 0) (D 1) (x 1) (y 1))
        ((A 0) (B 1) (C 1) (D 0) (x 1) (y 1))
        ((A 0) (B 1) (C 1) (D 1) (x 1) (y 1))
        ((A 1) (B 0) (C 0) (D 0) (x 1) (y 1))
        ((A 1) (B 0) (C 0) (D 1) (x 1) (y 1))
        ((A 1) (B 0) (C 1) (D 1) (x 1) (y 1))
        ((A 1) (B 1) (C 0) (D 0) (x 1) (y 1))
        ((A 1) (B 1) (C 0) (D 1) (x 1) (y 1))
        ((A 1) (B 1) (C 1) (D 0) (x 1) (y 1))
        ((A 1) (B 1) (C 1) (D 1) (x 1) (y 1))))
      (counter_examples ()))) |}]

let%expect_test "valid output with postcondition parses correctly" =
  print_output_from_string test_output_valid_pc ;
  [%expect
    {|
      (Ok
       ((flags (sat))
        (states
         (((0:EAX 0) (1:EAX 0)) ((0:EAX 0) (1:EAX 1)) ((0:EAX 1) (1:EAX 0))
          ((0:EAX 1) (1:EAX 1))))
        (witnesses (((0:EAX 0) (1:EAX 0))))
        (counter_examples
         (((0:EAX 0) (1:EAX 1)) ((0:EAX 1) (1:EAX 0)) ((0:EAX 1) (1:EAX 1)))))) |}]
