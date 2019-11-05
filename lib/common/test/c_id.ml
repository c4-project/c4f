(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Act_utils

let%expect_test "generator sample" =
  My_quickcheck.print_sample
    ~printer:(Fmt.pr "@[%a@]@." Act_common.C_id.pp)
    (module Act_common.C_id) ;
  [%expect
    {|
    B
    C
    E_aZEtaPErNE3s9NLjD_
    H_o
    MB
    MfFQD1
    QL5D7
    RnCa
    Sz_St8G0yCfqN
    Vw8AOgVIAbZa
    XPllbn_tGxObBDDm
    eR_S_89CxSN
    emNU_cG_TXvl_EIf_
    k1_Dt
    keU
    ob_Qhsc4hQAg799
    xOIW0_DQim6zTrFCUAT
    xZvJKQ2
    x_vx__m_kA
    yoU_2 |}]

let%test_unit "conversion to/from JSON is the identity" =
  Base_quickcheck.Test.run_exn
    (module Act_common.C_id)
    ~f:(fun id ->
      [%test_result: Act_common.C_id.t] ~here:[[%here]]
        Act_common.C_id.(t_of_yojson (yojson_of_t id))
        ~expect:id ~equal:[%equal: Act_common.C_id.t])

let%test_module "is_string_safe (standard)" =
  ( module struct
    open Act_common.C_id

    let test (candidate : string) : unit =
      Io.print_bool (is_string_safe candidate)

    let%expect_test "positive example" = test "t0r0" ; [%expect {| true |}]

    let%expect_test "positive (but not herd-safe) example" =
      test "_t0r0" ; [%expect {| true |}]

    let%expect_test "Herd program ID" = test "P45" ; [%expect {| true |}]

    let%expect_test "negative example" = test "0r0" ; [%expect {| false |}]
  end )

let%test_module "is_string_safe (Herd-safe)" =
  ( module struct
    open Act_common.C_id.Herd_safe

    let test (candidate : string) : unit =
      Io.print_bool (is_string_safe candidate)

    let%expect_test "positive example" = test "t0r0" ; [%expect {| true |}]

    let%expect_test "negative example" = test "_t0r0" ; [%expect {| false |}]

    let%expect_test "program ID (negative)" =
      test "P45" ; [%expect {| false |}]

    (* This particular candidate Herd identifier has been the bane of the ACT
       fuzzer for a little while. It clashes with an innocuous macro of the
       same name in Litmus-generated harness C files. *)
    let%expect_test "clashing 'N' (negative)" =
      test "N" ; [%expect {| false |}]
  end )
