(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to permit
   persons to whom the Software is furnished to do so, subject to the
   following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
   NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
   DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
   OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
   USE OR OTHER DEALINGS IN THE SOFTWARE. *)

open Core_kernel
open Act_common

let run_herd ?arch ?(argv = []) (_o : Output.t) (cfg : Act_config.Act.t) :
    unit Or_error.t =
  let herd = Act_config.Act.herd_or_default cfg in
  Act_sim_herd.Filter.run_direct ?arch herd argv

let herd_command : Command.t =
  Command.basic ~summary:"runs Herd"
    Command.Let_syntax.(
      let%map_open standard_args = Args.Standard.get
      and arch =
        choose_one
          [ Args.flag_to_enum_choice (Some Act_sim.Arch.C) "-c"
              ~doc:"Use the act.conf-configured model for C"
          ; map
              ~f:(Option.map ~f:(fun x -> Some (Act_sim.Arch.Assembly x)))
              (Args.arch
                 ~doc:
                   "Use the act.conf-configured model for this architecture"
                 ()) ]
          ~if_nothing_chosen:(`Default_to None)
      and argv =
        flag "--" Command.Flag.escape
          ~doc:"STRINGS Arguments to send to Herd directly."
      in
      fun () ->
        Common.lift_command standard_args ~with_compiler_tests:false
          ~f:(fun _args -> run_herd ?arch ?argv))

let run_litmus_locally ?(argv = []) (_o : Output.t) (cfg : Act_config.Act.t)
    : unit Or_error.t =
  let open Or_error.Let_syntax in
  let machines = Act_config.Act.machines cfg in
  let%bind machine =
    Act_config.Machine.Spec.Set.get machines Act_config.Machine.Id.default
  in
  let%bind litmus_cfg =
    Act_config.Machine.Spec.With_id.ensure_litmus machine
  in
  Act_sim_litmus.Filter.run_direct litmus_cfg argv

let litmus_command : Command.t =
  Command.basic ~summary:"runs Litmus locally"
    Command.Let_syntax.(
      let%map_open standard_args = Args.Standard.get
      and argv =
        flag "--" Command.Flag.escape
          ~doc:"STRINGS Arguments to send to Herd directly."
      in
      fun () ->
        Common.lift_command standard_args ~with_compiler_tests:false
          ~f:(fun _args -> run_litmus_locally ?argv))

let command : Command.t =
  Command.group ~summary:"runs external tools directly"
    [("herd", herd_command); ("litmus", litmus_command)]
