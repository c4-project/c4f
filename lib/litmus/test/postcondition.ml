(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Base_quickcheck
module Src = Act_litmus

let%expect_test "quickcheck generator sample" =
  Act_utils.My_quickcheck.print_sample
    ( module struct
      type t = int Src.Postcondition.t
      [@@deriving compare, sexp, quickcheck]
    end ) ;
  [%expect
    {|
    ((quantifier exists)
     (predicate (Bracket (Elt (Eq _FZtMt__9VQ_p_E__ -12135116520244)))))
    ((quantifier exists)
     (predicate (Bracket (Elt (Eq 2:w_3_a_9N___ -87945063396361262)))))
    ((quantifier exists)
     (predicate
      (Or (And (Elt (Eq 1:_vy__u -192459552073625)) (Elt (Eq ______ 1)))
       (Elt (Eq 4:O_R_mwz -1)))))
    ((quantifier exists)
     (predicate
      (And (Elt (Eq 6:O___w__jH___Q_R -122406473))
       (Elt (Eq h_bl___sR_N_9_ -793)))))
    ((quantifier exists) (predicate (Elt (Eq B 17389146807090))))
    ((quantifier exists) (predicate (Elt (Eq _8z7__j__ 263823))))
    ((quantifier exists) (predicate (Elt (Eq _hx -82934877441))))
    ((quantifier exists) (predicate (Elt (Eq r_C___PG_ 9020672024159))))
    ((quantifier exists) (predicate (Elt (Eq 0:__My 15568213580))))
    ((quantifier exists) (predicate (Elt (Eq 7:fp 30823683174765055))))
    ((quantifier forall)
     (predicate
      (Bracket
       (Or (Elt (Eq ___ 8))
        (Or (Elt (Eq __K_QA 381796369316705))
         (Elt (Eq 6:_t_g1nh2 -8267175828708256)))))))
    ((quantifier forall)
     (predicate
      (Or (Bracket (And (Elt (Eq n7 -28613698)) (Elt (Eq e 5450134))))
       (Bracket (Elt (Eq _5_ -6537442140))))))
    ((quantifier forall)
     (predicate
      (And (Elt (Eq 17:_m_W_b_d_x_E_fE6R_ -4611686018427387904))
       (Elt (Eq _X_Bl_aivN_E___H_e_ -590525)))))
    ((quantifier forall) (predicate (Elt (Eq T0_ghNl7k_bY_ 74323250521029))))
    ((quantifier forall) (predicate (Elt (Eq _F -12981610))))
    ((quantifier forall) (predicate (Elt (Eq _GV_Ji___ -644427380338))))
    ((quantifier forall) (predicate (Elt (Eq __S___y___e_6Ju__s -5))))
    ((quantifier forall) (predicate (Elt (Eq oZvJK -1))))
    ((quantifier forall) (predicate (Elt (Eq 5:___BV 76121054410))))
    ((quantifier forall)
     (predicate (Elt (Eq 12:H_R_g__KVWq192I_ 4611686018427387903)))) |}]

let%expect_test "pretty-printing example" =
  Src.Postcondition.(
    Fmt.pr "@[%a@]@." (pp ~pp_const:Int.pp)
      (make ~quantifier:Quantifier.Exists
         ~predicate:
           Pred.(
             Infix.(
               bracket
                 ( Act_common.Litmus_id.global_of_string_exn "foo" ==? 10
                 && Act_common.Litmus_id.global_of_string_exn "bar" ==? 30
                 )
               || Act_common.Litmus_id.global_of_string_exn "baz" ==? 50)))) ;
  [%expect {| exists ((foo == 10 /\ bar == 30) \/ baz == 50) |}]
