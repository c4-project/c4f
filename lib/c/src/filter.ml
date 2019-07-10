(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Stdio

module Pb = Plumbing

module Aux = struct
  type t = { seed: int option; o: Act_common.Output.t; config: Act_config.Fuzz.t }
end

include
  Pb.Filter.Make (struct
    type aux_i = Aux.t
    type aux_o = unit

    let name = "Mini-C explainer"

    let run (ctx : Aux.t Pb.Filter_context.t) (ic : In_channel.t) (oc : Out_channel.t)
        : unit Or_error.t =
      let {Aux.seed; o; config} = Pb.Filter_context.aux ctx in
      let input = Pb.Filter_context.input ctx in
      Or_error.(
        ic
        |> Act_c_lang.Frontend.Litmus.load_from_ic ~path:(Pb.Input.to_string input)
        >>= Act_c_mini.Convert.litmus_of_raw_ast
        >>= Fuzzer.run ?seed ~o ~config
        >>| Act_c_mini.Litmus.Pp.print oc
      )
  end)
