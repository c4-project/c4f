(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open C4f_utils

let%expect_test "generator sample" =
  My_quickcheck.print_sample
    ~printer:(Fmt.pr "@[%a@]@." C4f_common.C_id.pp)
    (module C4f_common.C_id) ;
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

let%expect_test "human generator sample" =
  My_quickcheck.print_sample
    ~printer:(Fmt.pr "@[%a@]@." C4f_common.C_id.pp)
    (module C4f_common.C_id.Human) ;
  [%expect
    {|
    easel_14
    foolish_asymptote_2
    foolish_heap
    fox_13
    game
    humble_television
    ibex
    jocular_easel
    llama
    llama_1
    llama_16
    loud_mouse
    modest_yurt
    opulent_yurt_0
    phone_16
    tall_house_2
    virtuous_aardvark_0
    virtuous_yurt
    xylophone
    zonal_juniper |}]

let%test_unit "conversion to/from JSON is the identity" =
  Base_quickcheck.Test.run_exn
    (module C4f_common.C_id)
    ~f:(fun id ->
      [%test_result: C4f_common.C_id.t] ~here:[[%here]]
        C4f_common.C_id.(t_of_yojson (yojson_of_t id))
        ~expect:id ~equal:[%equal: C4f_common.C_id.t] )

let%test_module "is_string_safe (standard)" =
  ( module struct
    open C4f_common.C_id

    let test (candidate : string) : unit =
      Io.print_bool (is_string_safe candidate)

    let%expect_test "positive example" = test "t0r0" ; [%expect {| true |}]

    let%expect_test "positive (but not herd-safe) example" =
      test "_t0r0" ; [%expect {| true |}]

    let%expect_test "Herd program ID" = test "P45" ; [%expect {| true |}]

    let%expect_test "negative example" = test "0r0" ; [%expect {| false |}]
  end )
