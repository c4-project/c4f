(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

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
      all_unit
        [ register_var
            (Act_common.Litmus_id.of_string "gen1")
            Act_fir.(
              Initialiser.make
                ~ty:Type.(int ~is_pointer:true ~is_atomic:true ())
                ~value:(Act_fir.Constant.int 1337))
        ; register_var
            (Act_common.Litmus_id.of_string "gen2")
            Act_fir.(
              Initialiser.make
                ~ty:Type.(int ~is_pointer:true ~is_atomic:true ())
                ~value:(Act_fir.Constant.int (-55)))
        ; register_var
            (Act_common.Litmus_id.of_string "gen3")
            Act_fir.(
              Initialiser.make
                ~ty:Type.(int ~is_pointer:true ~is_atomic:false ())
                ~value:(Act_fir.Constant.int 1998))
        ; register_var
            (Act_common.Litmus_id.of_string "gen4")
            Act_fir.(
              Initialiser.make
                ~ty:Type.(int ~is_pointer:true ~is_atomic:false ())
                ~value:(Act_fir.Constant.int (-4))) ])
end
