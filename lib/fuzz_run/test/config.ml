(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

let%test_module "Weight summaries" =
  ( module struct
    let%expect_test "pp: example summary" =
      let value =
        Src.Summary.Adjusted.(Adjusted {original= 27; actual= 53})
      in
      let readme =
        {|
        Why, hello there!  This is an example README.

        It is very long...

        ...and has line breaks and other such interesting things in it.
        Hopefully, it'll be enough to be able to test that the action
        summary pretty-printer does what it's supposed to.

        Be seeing you.
      |}
      in
      let map =
        Map.singleton
          (module Common.Id)
          (Common.Id.of_string "foo")
          {Src.Summary.value; readme}
      in
      Fmt.pr "%a@." Src.Config.Weight_summary.pp map ;
      [%expect
        {|
      foo:
        Weight: 53x (normally 27x)
        Summary:
          Why, hello there! This is an example README.

          It is very long...

          ...and has line breaks and other such interesting things in it.
          Hopefully, it'll be enough to be able to test that the action summary
          pretty-printer does what it's supposed to.

          Be seeing you. |}]
  end )
