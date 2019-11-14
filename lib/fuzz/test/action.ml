(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Src = Act_fuzz
open Act_fuzz.Action

module Test_utils = struct
  let reify_test (test : Src.Subject.Test.t) :
      Act_c_lang.Ast.Translation_unit.t Src.State.Monad.t =
    Src.State.Monad.with_vars (fun vars ->
        List.mapi (Act_litmus.Test.Raw.threads test) ~f:(fun id p ->
            let fn = Src.Subject.Thread.to_function ~vars ~id p in
            Act_c_mini.(Reify.func (Named.name fn) (Named.value fn))))

  let run_and_dump_test (action : Src.Subject.Test.t Src.State.Monad.t)
      ~(initial_state : Src.State.t) : unit =
    let r = Src.State.Monad.(run (action >>= reify_test) initial_state) in
    Fmt.(
      pr "%a@."
        (result ~ok:(list Act_c_lang.Ast.External_decl.pp) ~error:Error.pp))
      r
end

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
