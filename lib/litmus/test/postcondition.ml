(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Base_quickcheck
module Src = C4f_litmus

let print_pred (p : int Src.Postcondition.t) : unit =
  Fmt.pr "@[%a@]@." (Src.Postcondition.pp ~pp_const:Int.pp) p

let%expect_test "quickcheck generator sample" =
  C4f_utils.My_quickcheck.print_sample ~printer:print_pred
    ( module struct
      type t = int Src.Postcondition.t [@@deriving compare, sexp, quickcheck]
    end ) ;
  [%expect
    {|
    exists
    ((e6hdrDsXhhi == -5456210478730345 /\
      (c___fm_D == 43767867115 \/ 2:c1Bi == -59305141083420128) /\
      11:gF2t59L0_BE7U == -14208201599242415 /\ true)
     \/ true \/
     (14:GlhQ25x8E8C2__v == 3 /\
      (12:wCz0rqSDVOaoiE == -697 \/
       (12:G == -10870525317272 \/
        (true \/
         ((false \/ gyjbiB == -48) /\ false /\ U_947w2Zs == -52395504246 /\ true)
         \/ (10:lJiB == -8 \/ (true /\ false)))))))
    exists
    ((9:XA6U1C == 19959961545163 /\ false /\ true) \/
     (((J == 28645920 \/
        (6:n5yi_7 == 42579 /\
         (((1:zV == 8065 \/ true \/ C == 57351232418) /\ false /\ 2:b ==
           -7487810475)
          \/ true)
         /\
         (((2:q1bn == -400675887355 \/ true) /\ (false \/ false)) \/
          (false /\
           (4:EOtKn == -82934877441 \/
            (N == -10688053957094 /\ false /\ Jx__ == -2)))))
        \/ QBQRQa_h == 215744077123198440)
       /\ 9:_ == 1484173655652007440)
      \/ 10:Oz1 == -15148861433))
    exists (true \/ (false \/ true \/ false))
    exists (rkQhQAg79 == -4790772421943367 \/ (false /\ true))
    exists
    ((ie91540maB == 610314293669845 \/
      (aBXQp5JPb == -66520238236796 /\ 7:KUTXtT_aZ == -268834788))
     /\ true /\ true)
    exists (true /\ false /\ true)
    exists
    (false /\
     (false /\
      (true /\
       ((lXacmv == 0 /\
         (10:H3R3_shTt3zA == -16291850538722160 /\
          (K0nD1Vhy == -314 \/
           (((false \/
              (true \/ h__8Q4 == 18102740624678708 \/
               ((0:ew == 742 \/ true \/
                 ((r == -4611686018427387904 /\ 0:F == -1) \/ 0:vj ==
                  61380856288))
                /\ true /\ M5 == -32826012517834798)))
             /\ (kTtB__l == 0 \/ false))
            \/ (_ == 933764513907491 \/ false)))))
        \/ true))))
    exists
    (P0ddM_QDnO == -312902695289958819 /\
     ((10:wlhN_82T_0y == 7298892 /\ true) \/ LcUJu_HB == 33402592557772))
    exists (S_9 == -162895976 /\ false)
    exists (B == 17389146807090)
    exists (IM_ == 15568213580)
    exists (0:kU_6Xb == 215397670475053354)
    exists (7:t_x_IXu_wyCq5G_bDn == -4611686018427387904)
    exists (15:oOY39QD_pA7ZV__3Ol == -37143150983259)
    forall (true \/ itTPYa_d_a8 == -216380089157031)
    forall (e5__ == -225523 \/ true)
    forall
    ((((true \/ (o == -138 \/ (false \/ 1:V == 2152528))) /\
       (3:_7fF == -1700373386570163572 \/
        ((false /\ true) \/ (1:_a == -449258 \/ false)))
       /\ 3:yfnZq == 180635663955347)
      \/ false)
     /\ (false /\ l0MSL == 1) /\ d == 777234762453557 /\ wQO_vx_ == 2643781)
    forall (true /\ true)
    forall (true)
    forall (_F == -12981610) |}]

let%expect_test "pretty-printing example" =
  Src.Postcondition.(
    Fmt.pr "@[%a@]@." (pp ~pp_const:Int.pp)
      (make ~quantifier:Quantifier.Exists
         ~predicate:
           Src.Predicate.(
             Infix.(
               C4f_common.Litmus_id.global_of_string_exn "foo" ==? 10
               && C4f_common.Litmus_id.global_of_string_exn "bar" ==? 30
               || C4f_common.Litmus_id.global_of_string_exn "baz" ==? 50)) )) ;
  [%expect {| exists ((foo == 10 /\ bar == 30) \/ baz == 50) |}]
