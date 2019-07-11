(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Act_fuzz.Action

let%test_module "Summary" =
  ( module struct
    open Act_fuzz.Action.Summary

    let%expect_test "pp: example summary" =
      let weight = Adjusted_weight.(Adjusted {original= 27; actual= 53}) in
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
      let summary = Act_fuzz.Action.Summary.make ~weight ~readme in
      Fmt.pr "%a@." pp summary ;
      [%expect
        {|
      Weight: 53x (normally 27x)
      Summary:
        Why, hello there! This is an example README.

        It is very long...

        ...and has line breaks and other such interesting things in it. Hopefully,
        it'll be enough to be able to test that the action summary pretty-printer
        does what it's supposed to.

        Be seeing you. |}]
  end )
