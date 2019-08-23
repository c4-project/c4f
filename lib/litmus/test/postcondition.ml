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
     (predicate
      (Bracket
       (Or (Elt (Eq 16:J 902895501375878957))
        (Elt (Eq yO0ddM_QDnOPbwHT 394320765567294))))))
    ((quantifier exists) (predicate (Bracket (Elt (Eq 6:kzHwOt9 -1)))))
    ((quantifier exists)
     (predicate
      (Or
       (Bracket
        (Or (Bracket (Elt (Eq 7:bWp71sSh 90965132575962503)))
         (Elt (Eq gl 177816))))
       (Bracket (Elt (Eq 8:PU_ 69829377413441892))))))
    ((quantifier exists)
     (predicate
      (Or (Elt (Eq wP_KR_ 10))
       (Or (Elt (Eq Y_m_ -1)) (Elt (Eq x__Z -249567708890932))))))
    ((quantifier exists)
     (predicate
      (And
       (Bracket
        (Or (Bracket (Elt (Eq ni 0)))
         (And (Elt (Eq 0:vK 381)) (Elt (Eq gU 2374814964917089553)))))
       (Bracket (Elt (Eq 2:_qL 257102))))))
    ((quantifier exists) (predicate (Elt (Eq B 17389146807090))))
    ((quantifier exists) (predicate (Elt (Eq RXtT_aZEtaPErNE3 -244))))
    ((quantifier exists) (predicate (Elt (Eq ib_Qh 1659469926793563215))))
    ((quantifier exists) (predicate (Elt (Eq 3:IM_ 15568213580))))
    ((quantifier exists) (predicate (Elt (Eq 3:p_5 1020929039755))))
    ((quantifier exists) (predicate (Elt (Eq 8:xtKnhx_m1LKJGp -349))))
    ((quantifier forall)
     (predicate
      (Bracket
       (Or (Bracket (Elt (Eq 5:XCxSNsl -6353388)))
        (Elt (Eq O_CUII_ 1510397937673))))))
    ((quantifier forall)
     (predicate
      (Bracket
       (And (Bracket (Elt (Eq tnuWPAc 278966333774708263)))
        (Bracket (Elt (Eq V_glvVSx89154 -12135116520244)))))))
    ((quantifier forall) (predicate (Bracket (Elt (Eq LoAm_w8 815989604)))))
    ((quantifier forall) (predicate (Bracket (Elt (Eq hS 4036446942)))))
    ((quantifier forall)
     (predicate
      (Or (Elt (Eq iJfhXdi8F2t59L -2035868215198038645))
       (And
        (And (Elt (Eq 17:ebJ_L__N_y_k_4Qjw7 -44)) (Elt (Eq 11:bx_a_XaXKCz 0)))
        (Elt (Eq bEZsajE6nffJfeafn -9937405399042326))))))
    ((quantifier forall)
     (predicate
      (And (Bracket (Elt (Eq 6:e3Qim6zTrFCU -3)))
       (Bracket
        (Or (Elt (Eq 8:U0 -15)) (Elt (Eq 11:eceGILQihEl 82012054457070)))))))
    ((quantifier forall) (predicate (Elt (Eq _F -12981610))))
    ((quantifier forall) (predicate (Elt (Eq ue_CGHy8R8 23890860676656021))))
    ((quantifier forall) (predicate (Elt (Eq z -2)))) |}]

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
