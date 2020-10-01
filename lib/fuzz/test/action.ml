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

  let run_and_dump_test (action : Src.Subject.Test.t Src.State.Monad.t)
      ~(initial_state : Src.State.t) : unit =
    let r = Src.State.Monad.(run (action >>= reify_test_m) initial_state) in
    Fmt.(pr "@[<v>%a@]@." (result ~ok:pp_tu ~error:Error.pp)) r

  let pp_vars :
      (Act_common.C_id.t, Act_fir.Constant.t option) List.Assoc.t Fmt.t =
    Fmt.(
      list ~sep:sp
        (pair ~sep:(any "=") Act_common.C_id.pp (option Act_fir.Constant.pp)))

  let run_and_dump_vars (action : Src.Subject.Test.t Src.State.Monad.t)
      ~(predicates : (Src.Var.Record.t -> bool) list)
      ~(scope : Act_common.Scope.t) ~(initial_state : Src.State.t) : unit =
    let result =
      Or_error.(
        Src.State.Monad.(run' action initial_state)
        >>| Accessor.(get (Tuple2.fst @> Src.State.vars))
        >>| Src.Var.Map.env_satisfying_all ~scope ~predicates
        >>| Map.to_alist
        >>| Travesty_base_exts.Alist.map_right
              ~f:(Accessor.get_option Act_fir.Env.Record.known_value))
    in
    Fmt.(pr "@[%a@]@." (result ~error:Error.pp ~ok:pp_vars)) result

  let run_and_dump_global_deps
      (action : Src.Subject.Test.t Src.State.Monad.t)
      ~(initial_state : Src.State.t) : unit =
    run_and_dump_vars action ~initial_state ~scope:Act_common.Scope.Global
      ~predicates:[Src.Var.Record.has_dependencies]
end
