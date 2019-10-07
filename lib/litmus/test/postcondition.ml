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

let print_pred (p : int Src.Postcondition.t) : unit =
  Fmt.pr "@[%a@]@." (Src.Postcondition.pp ~pp_const:Int.pp) p

let%expect_test "quickcheck generator sample" =
  Act_utils.My_quickcheck.print_sample ~printer:print_pred
    ( module struct
      type t = int Src.Postcondition.t
      [@@deriving compare, sexp, quickcheck]
    end ) ;
  [%expect
    {|
    exists
    (((false /\ 5:byoAm_ == 4611686018427387903) \/ J == -310408308011763050) /\
     (false \/ Q8A80zO == -202))
    exists
    ((true \/ (true \/ (true /\ (true /\ true)))) /\ FmwYRE_nE == 0 /\
     (__Z__vxCA8W == -240271323391785638 /\ false) /\
     ((((((true /\ TPglv == 487401135026 /\ false) \/
          (false /\ X == -38775884510 /\ false))
         /\ _C4Ox6Ju == -6)
        \/ ((false \/ (R__79to == -244 \/ true)) /\ 7:uA6U1C == 183))
       /\ (false /\ A == -4611686018427387904))
      \/
      (true /\ (AJyi_7Du == 167403080690 \/ (true /\ true)) /\
       (9:kWq192IL == -33093850200 /\
        (((((true \/
             (true /\ rc == 102900814189760 /\ (false \/ 1:nx == 19549488)))
            /\ (false /\ 3:rwN == -5897793281918))
           \/ (3:ncG == -2131178611 \/ 3:vn1P == -9365962))
          /\
          ((((false \/ (1:hl == 4967323952 /\ false)) /\
             (false \/ false \/ true))
            \/ false)
           /\ ((true /\ g_Vz == 207) \/ true)))
         \/ true \/ true))))
     /\ true)
    exists (4:_ == 257102 /\ false)
    exists (false)
    exists (B == 17389146807090)
    exists (ihsc4hQA == 1659469926793563215)
    exists (1:K_D87VA == 244206559873837)
    exists (3:IM_ == 15568213580)
    exists (3:p_5 == 1020929039755)
    exists (5:DAUx_ == -79841844)
    exists (5:Vtm4K3Gxx_a_XaXKC == 135613173070458503)
    exists (15:MbJ_L__N_y_k_4Qjw7_ == 3535273278795)
    forall
    (((true \/
       (14:F_2okooCLO2_8Q4 == 16286370291 /\ pVZ07G6bRB_j_ == 47236649163052) \/
       false)
      /\ 16:Qa_YCHgroTw15L == 0)
     \/ (11:rPV_fm_Dji2Wm == 290908867 \/ I == 313 \/ 15:L == 25123272713) \/
     true)
    forall ((4:z == -11912604273 /\ DRxjS_ == 15003145510372) \/ true)
    forall (true \/ sOtUZE == 3)
    forall (Ci8Qu_V5QJ9L0_B == 2936284940 \/ (false /\ true))
    forall
    (1:Jn_E7w2ZsajE6 == -4151781669626850975 \/ 14:RSS5EzM8V == 11752254098526)
    forall (YS_89CxSNsl == -15805871 /\ true /\ true)
    forall (false /\ false)
    forall (_F == -12981610) |}]

let%expect_test "pretty-printing example" =
  Src.Postcondition.(
    Fmt.pr "@[%a@]@." (pp ~pp_const:Int.pp)
      (make ~quantifier:Quantifier.Exists
         ~predicate:
           Pred.(
             Infix.(
               Act_common.Litmus_id.global_of_string_exn "foo" ==? 10
               && Act_common.Litmus_id.global_of_string_exn "bar" ==? 30
               || Act_common.Litmus_id.global_of_string_exn "baz" ==? 50)))) ;
  [%expect {| exists ((foo == 10 /\ bar == 30) \/ baz == 50) |}]
