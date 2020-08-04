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
  let reify_test (test : Src.Subject.Test.t)
      (vars : Src.Var.Record.t Act_common.Scoped_map.t) :
      Act_litmus_c.Ast.Translation_unit.t =
    List.mapi (Act_litmus.Test.Raw.threads test) ~f:(fun id p ->
        let fn = Src.Subject.Thread.to_function ~vars ~id p in
        Act_fir.(
          Reify.func
            (Act_common.C_named.name fn)
            (Act_common.C_named.value fn)))

  let pp_tu : Act_litmus_c.Ast.Translation_unit.t Fmt.t =
    Fmt.(list ~sep:(sp ++ sp) Act_litmus_c.Ast.External_decl.pp)

  let reify_test_m (test : Src.Subject.Test.t) :
      Act_litmus_c.Ast.Translation_unit.t Src.State.Monad.t =
    Src.State.Monad.with_vars (reify_test test)

  let run_and_dump_test (action : Src.Subject.Test.t Src.State.Monad.t)
      ~(initial_state : Src.State.t) : unit =
    let r = Src.State.Monad.(run (action >>= reify_test_m) initial_state) in
    Fmt.(pr "@[<v>%a@]@." (result ~ok:pp_tu ~error:Error.pp)) r

  let run_and_dump_global_deps
      (action : Src.Subject.Test.t Src.State.Monad.t)
      ~(initial_state : Src.State.t) : unit =
    let r =
      Or_error.(
        Src.State.Monad.run' action initial_state
        >>| fst
        >>| Src.State.vars_satisfying_all ~scope:Global
              ~predicates:[Src.Var.Record.has_dependencies])
    in
    Fmt.(pr "%a@." (result ~ok:(list Act_common.C_id.pp) ~error:Error.pp)) r
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
