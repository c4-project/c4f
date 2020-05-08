(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module Src = Act_fuzz
end

module Test_common = struct
  let prepare_fuzzer_state () : unit Src.State.Monad.t =
    Src.State.Monad.(
      register_var
        (Act_common.Litmus_id.of_string "gen1")
        Act_c_mini.(
          Initialiser.make
            ~ty:Type.(int ~is_pointer:true ~is_atomic:true ())
            ~value:(Act_c_mini.Constant.int 1337)
            ())
      >>= fun () ->
      register_var
        (Act_common.Litmus_id.of_string "gen2")
        Act_c_mini.(
          Initialiser.make
            ~ty:Type.(int ~is_pointer:true ~is_atomic:true ())
            ~value:(Act_c_mini.Constant.int (-55))
            ()))

  let pp_vars :
      (Act_common.C_id.t, Act_c_mini.Constant.t option) List.Assoc.t Fmt.t =
    Fmt.(
      list ~sep:sp
        (pair ~sep:(any "=") Act_common.C_id.pp
           (option Act_c_mini.Constant.pp)))

  let run_and_dump_vars (test_action : Src.Subject.Test.t Src.State.Monad.t)
      ~(predicates : (Src.Var.Record.t -> bool) list)
      ~(initial_state : Src.State.t) : unit =
    let result =
      Or_error.(
        Src.State.Monad.(run' test_action initial_state)
        >>| fst >>| Src.State.vars
        >>| Src.Var.Map.env_satisfying_all ~scope:(Local 0) ~predicates
        >>| Map.to_alist
        >>| Travesty_base_exts.Alist.map_right
              ~f:Act_c_mini.Env.Record.known_value)
    in
    Fmt.(pr "@[%a@]@." (result ~error:Error.pp ~ok:pp_vars)) result

  let run_and_dump_globals =
    run_and_dump_vars ~predicates:[Src.Var.Record.is_global]

  let run_and_dump_kvs =
    run_and_dump_vars ~predicates:[Src.Var.Record.has_known_value]

  let run_and_dump_deps =
    run_and_dump_vars ~predicates:[Src.Var.Record.has_dependencies]
end
