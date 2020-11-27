(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

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

let pp_unit : Act_litmus_c.Ast.Translation_unit.t Fmt.t =
  Fmt.(vbox (pp_prelude ++ Act_litmus_c.Ast.Translation_unit.pp))

let c_of_output : Output.t -> Act_litmus_c.Ast.Translation_unit.t =
  Fn.compose Act_litmus_c.Reify.program Output.program

let pp_del : Output.t Fmt.t = Fmt.(using c_of_output pp_unit)

let delitmusify_and_print (test : Act_fir.Litmus.Test.t)
    (oc : Stdio.Out_channel.t) ~(config : Config.t) : Output.t Or_error.t =
  let (module R) = Config.to_runner config in
  Or_error.Let_syntax.(
    let%map dl = R.run test in
    Act_utils.My_format.fdump oc (Fmt.vbox pp_del) dl ;
    dl)

let run (config : Config.t) (input : Plumbing.Input.t) (output : Plumbing.Output.t)
  : Output.t Or_error.t =
  Plumbing.Io_helpers.with_input_and_output input output ~f:(fun ic oc ->
    Or_error.Let_syntax.(
      let%bind vast =
        Act_litmus_c.Frontend.Fir.load_from_ic
          ~path:(Pb.Input.to_string input)
          ic
      in
      delitmusify_and_print vast oc ~config))
