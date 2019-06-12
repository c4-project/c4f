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
  [ "// <!> Auto-generated from a litmus test by act."
  ; "#include <stdatomic.h>"
  ; "" ]

let pp_prelude : unit Fmt.t =
  Fmt.(const (vbox (list ~sep:sp string)) prelude)

let pp_del : Output.t Fmt.t =
  Fmt.(
    prefix pp_prelude
      (using
         (Fn.compose Act_c.Mini_reify.program Output.program)
         (vbox Act_c_lang.Ast.Translation_unit.pp)))

let delitmusify_and_print (vast : Act_c.Mini_litmus.Ast.Validated.t) (oc : Stdio.Out_channel.t) :
  Output.Aux.t Or_error.t =
  Or_error.Let_syntax.(
    let%map dl = Runner.run vast in
    Fmt.pf (Caml.Format.formatter_of_out_channel oc) "%a@." pp_del dl ;
    Output.aux dl
  )

include Pb.Filter.Make (struct
    type aux_i = unit
    type aux_o = Output.Aux.t

    let name = "delitmus"
    let tmp_file_ext _ = "c"

    let run (ctx : unit Pb.Filter_context.t) ic oc :
      aux_o Or_error.t =
      let input = Pb.Filter_context.input ctx in
      Or_error.Let_syntax.(
        let%bind ast =
          Act_c_lang.Frontend.Litmus.load_from_ic ~path:(Pb.Input.to_string input) ic
        in
        let%bind vast = Act_c.Mini_convert.litmus_of_raw_ast ast in
        delitmusify_and_print vast oc)
  end)
