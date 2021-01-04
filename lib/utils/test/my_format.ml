(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open C4f_utils.My_format

let%test_module "pp_c_braces" =
  ( module struct
    let%expect_test "short items" =
      Fmt.(
        pr "@[%a@]@."
          (pp_c_braces (fun f () ->
               string f "eggs" ; sp f () ; string f "ham"))
          ()) ;
      [%expect {| { eggs ham } |}]

    let%expect_test "things before and after" =
      Fmt.(
        pr "@[poached@ %a@ sandwich@]@."
          (pp_c_braces (fun f () ->
               string f "eggs" ; sp f () ; string f "ham"))
          ()) ;
      [%expect {| poached { eggs ham } sandwich |}]

    let%expect_test "long items" =
      Fmt.(
        pr "@[%a@]@."
          (pp_c_braces (fun f () ->
               string f "a very long string that'll doubtless wrap the box" ;
               sp f () ;
               string f "ham and cheese and cheese and ham"))
          ()) ;
      [%expect
        {|
    {
        a very long string that'll doubtless wrap the box
        ham and cheese and cheese and ham
    } |}]

    let%expect_test "long items; things before and after" =
      Fmt.(
        pr "@[this is@ %a@ and cheese@]@."
          (pp_c_braces (fun f () ->
               string f "a very long string that'll doubtless wrap the box" ;
               sp f () ;
               string f "ham and cheese and cheese and ham"))
          ()) ;
      [%expect
        {|
    this is
    {
        a very long string that'll doubtless wrap the box
        ham and cheese and cheese and ham
    } and cheese |}]
  end )
