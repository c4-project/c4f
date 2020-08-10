(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

module Filter :
  Plumbing.Filter_types.S with type aux_i = unit and type aux_o = unit =
Plumbing.Filter.Make (struct
  let name = "dump-stats"

  type aux_i = unit

  type aux_o = unit

  let run (ctx : aux_i Plumbing.Filter_context.t) (ic : Stdio.In_channel.t)
      (oc : Stdio.Out_channel.t) : aux_o Or_error.t =
    Or_error.Let_syntax.(
      let%map test =
        Frontend.Fir.load_from_ic ic
          ~path:(Plumbing.Filter_context.input_path_string ctx)
      in
      Act_fir.Litmus_stats.(
        Act_utils.My_format.fdump oc (Fmt.vbox Statset.pp) (scrape test)))
end)
