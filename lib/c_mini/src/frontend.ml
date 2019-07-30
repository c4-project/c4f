(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

include Plumbing.Loadable.Make_chain (Act_c_lang.Frontend.Litmus) (struct
    type dst = Litmus.Ast.Validated.t
    let f = Convert.litmus_of_raw_ast
  end)
