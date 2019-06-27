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

open Act_utils.My_format

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
