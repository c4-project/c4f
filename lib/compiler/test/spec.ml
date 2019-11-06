(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

let%test_module "Pretty-printing without ID" =
  ( module struct
    type t = Act_compiler.Spec.t

    let enabled_example : t = Lazy.force Data.Spec_sets.gcc_spec

    let disabled_example : t = Lazy.force Data.Spec_sets.disabled_gcc_spec

    let test ~(is_verbose : bool) : t -> unit =
      Fmt.pr "@[%a@]@." (Act_compiler.Spec.pp_verbose is_verbose)

    let%expect_test "enabled compiler: verbose" =
      test ~is_verbose:true enabled_example ;
      [%expect
        {|
      Enabled: true
      Style: gcc
      Architecture: x86.att
      Command: gcc |}]

    let%expect_test "enabled compiler: not verbose" =
      test ~is_verbose:false enabled_example ;
      [%expect {|
        gcc x86.att enabled |}]

    let%expect_test "disabled compiler: verbose" =
      test ~is_verbose:true disabled_example ;
      [%expect
        {|
      Enabled: false
      Style: gcc
      Architecture: x86.att
      Command: gcc |}]

    let%expect_test "disabled compiler: not verbose" =
      test ~is_verbose:false disabled_example ;
      [%expect {|
        gcc x86.att disabled |}]
  end )
