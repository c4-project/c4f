(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module Src = C4f_fuzz
end

module Test_common = struct
  let prepare_fuzzer_state () : unit Src.State.Monad.t =
    Src.State.Monad.(
      all_unit
        [ register_var
            (C4f_common.Litmus_id.of_string "gen1")
            C4f_fir.
              { Initialiser.ty= Type.(int ~is_pointer:true ~is_atomic:true ())
              ; value= C4f_fir.Constant.int 1337 }
        ; register_var
            (C4f_common.Litmus_id.of_string "gen2")
            C4f_fir.
              { Initialiser.ty= Type.(int ~is_pointer:true ~is_atomic:true ())
              ; value= C4f_fir.Constant.int (-55) }
        ; register_var
            (C4f_common.Litmus_id.of_string "gen3")
            C4f_fir.
              { Initialiser.ty=
                  Type.(int ~is_pointer:true ~is_atomic:false ())
              ; value= C4f_fir.Constant.int 1998 }
        ; register_var
            (C4f_common.Litmus_id.of_string "gen4")
            C4f_fir.
              { Initialiser.ty=
                  Type.(int ~is_pointer:true ~is_atomic:false ())
              ; value= C4f_fir.Constant.int (-4) } ] )
end
