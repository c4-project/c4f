(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Pb = Plumbing

let prelude : string list =
  [ "// <!> Auto-generated from a litmus test by ACT."
  ; "#include <stdatomic.h>"
  ; "#include <stdbool.h>"
  ; "" ]

let pp_prelude (type a) : a Fmt.t = Fmt.(const (list ~sep:sp string) prelude)

let pp_unit : Act_c_lang.Ast.Translation_unit.t Fmt.t =
  Fmt.(vbox (pp_prelude ++ Act_c_lang.Ast.Translation_unit.pp))

let c_of_output : Output.t -> Act_c_lang.Ast.Translation_unit.t =
  Fn.compose Act_fir.Reify.program Output.program

let pp_del : Output.t Fmt.t = Fmt.(using c_of_output pp_unit)

let delitmusify_and_print (test : Act_fir.Litmus.Test.t)
    (oc : Stdio.Out_channel.t) ~(config : Config.t) : Output.t Or_error.t =
  let (module R) = Config.to_runner config in
  Or_error.Let_syntax.(
    let%map dl = R.run test in
    Fmt.pf (Caml.Format.formatter_of_out_channel oc) "%a@." pp_del dl ;
    dl)

include Pb.Filter.Make (struct
  type aux_i = Config.t

  type aux_o = Output.t

  let name = "delitmus"

  let run (ctx : Config.t Pb.Filter_context.t) ic oc : aux_o Or_error.t =
    let config = Pb.Filter_context.aux ctx in
    let input = Pb.Filter_context.input ctx in
    Or_error.Let_syntax.(
      let%bind vast =
        Act_fir.Frontend.load_from_ic ~path:(Pb.Input.to_string input) ic
      in
      delitmusify_and_print vast oc ~config)
end)
