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

let pp_prelude (type a) : a Fmt.t =
  Fmt.(const (vbox (list ~sep:sp string)) prelude)

let pp_del : Output.t Fmt.t =
  Fmt.(
    pp_prelude
    ++ using
         (Fn.compose Act_c_mini.Reify.program Output.program)
         (vbox Act_c_lang.Ast.Translation_unit.pp))

let delitmusify_and_print (vast : Act_c_mini.Litmus.Ast.Validated.t)
    (oc : Stdio.Out_channel.t) ~(style : Runner.Style.t) : Aux.t Or_error.t
    =
  Or_error.Let_syntax.(
    let%map dl = Runner.run vast ~style in
    Fmt.pf (Caml.Format.formatter_of_out_channel oc) "%a@." pp_del dl ;
    Output.aux dl)

include Pb.Filter.Make (struct
  type aux_i = Runner.Style.t

  type aux_o = Aux.t

  let name = "delitmus"

  let run (ctx : Runner.Style.t Pb.Filter_context.t) ic oc :
      aux_o Or_error.t =
    let style = Pb.Filter_context.aux ctx in
    let input = Pb.Filter_context.input ctx in
    Or_error.Let_syntax.(
      let%bind vast =
        Act_c_mini.Frontend.load_from_ic ~path:(Pb.Input.to_string input) ic
      in
      delitmusify_and_print vast oc ~style)
end)
