(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

module Test_utils = struct
  let reify_test (test : Src.Subject.Test.t)
      (vars : Src.Var.Record.t Act_common.Scoped_map.t) :
      Act_litmus_c.Ast.Translation_unit.t =
    List.mapi (Act_litmus.Test.Raw.threads test) ~f:(fun id p ->
        let fn = Src.Subject.Thread.to_function ~vars ~id p in
        Act_litmus_c.Reify.func fn)

  let pp_tu : Act_litmus_c.Ast.Translation_unit.t Fmt.t =
    Fmt.(list ~sep:(sp ++ sp) Act_litmus_c.Ast.External_decl.pp)

  let reify_test_m (test : Src.Subject.Test.t) :
      Act_litmus_c.Ast.Translation_unit.t Src.State.Monad.t =
    Src.State.Monad.with_vars (reify_test test)

  let pp_vars : Src.Var.Map.t Fmt.t =
    Fmt.(
      vbox ~indent:2
      (any "Vars:" ++ cut ++ Common.Scoped_map.pp Src.Var.Record.pp)
    )

  let run_and_dump_test (action : Src.Subject.Test.t Src.State.Monad.t)
      ~(initial_state : Src.State.t) : unit =
    let r = Or_error.(
      Src.State.Monad.(run' (action >>= reify_test_m) initial_state)
      >>| (fun (state, tu) -> (tu, state.@(Src.State.vars)))
    )
    in
    Fmt.(pr "@[<v>%a@]@." (result ~ok:(pair ~sep:(cut++cut) pp_tu pp_vars) ~error:Error.pp)) r
end
