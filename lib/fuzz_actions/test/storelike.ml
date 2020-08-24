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
        Act_fir.(
          Initialiser.make
            ~ty:Type.(int ~is_pointer:true ~is_atomic:true ())
            ~value:(Act_fir.Constant.int 1337))
      >>= fun () ->
      register_var
        (Act_common.Litmus_id.of_string "gen2")
        Act_fir.(
          Initialiser.make
            ~ty:Type.(int ~is_pointer:true ~is_atomic:true ())
            ~value:(Act_fir.Constant.int (-55))))

  let run_and_dump_vars =
    Act_fuzz_test.Action.Test_utils.run_and_dump_vars ~scope:(Local 0)

  let run_and_dump_globals =
    run_and_dump_vars ~predicates:[Src.Var.Record.is_global]

  let run_and_dump_kvs =
    run_and_dump_vars ~predicates:[Src.Var.Record.has_known_value]

  let run_and_dump_deps =
    run_and_dump_vars ~predicates:[Src.Var.Record.has_dependencies]
end
