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

let run_backend ?(arch = Act_sim.Arch.C) ?(fqid : Id.t = Id.of_string "herd")
    (_o : Output.t) (cfg : Act_config.Act.t) : unit Or_error.t =
  let module Res = Sim_support.Make_resolver (struct
    let cfg = cfg
  end) in
  Or_error.Let_syntax.(
    let%bind (module Sim) = Res.resolve_single fqid in
    Sim.Filter.run arch (Plumbing.Input.stdin ()) Plumbing.Output.stdout
    )

let run_command : Command.t =
  Command.basic ~summary:"runs a configured test backend"
    Command.Let_syntax.(
      let%map_open standard_args = Args.Standard.get
      and sim = Args.simulator ()
      and arch =
        choose_one
          [ Args.flag_to_enum_choice (Some Act_sim.Arch.C) "-c"
              ~doc:"Tells the backend to test the input against the C memory model"
          ; map
              ~f:(Option.map ~f:(fun x -> Some (Act_sim.Arch.Assembly x)))
              (Args.arch
                 ~doc:
                   "ARCH tells the backend to test the input against the given architecture"
                 ()) ]
          ~if_nothing_chosen:(`Default_to None)
      in
      fun () ->
        Common.lift_command standard_args ~with_compiler_tests:false
          ~f:(fun _args -> run_backend ?arch ?fqid:sim))

let parse_backend ?(fqid : Id.t = Id.of_string "herd")
    (_o : Output.t) (cfg : Act_config.Act.t) : unit Or_error.t =
  let module Res = Sim_support.Make_resolver (struct
    let cfg = cfg
  end) in
  Or_error.Let_syntax.(
    let%bind (module Sim) = Res.resolve_single fqid in
    let%bind out = Sim.Reader.load_from_isrc (Plumbing.Input.stdin ()) in
    let%map obs = Act_sim.Output.to_observation_or_error ~handle_skipped:`Error out in
    Yojson.Safe.pretty_to_channel Stdio.Out_channel.stdout
      (Act_sim.Output.Observation.to_yojson obs)
    )

let parse_command : Command.t =
  Command.basic ~summary:"parses native output from a configured test backend"
    Command.Let_syntax.(
      let%map_open standard_args = Args.Standard.get
      and sim = Args.simulator ()
      in
      fun () ->
        Common.lift_command standard_args ~with_compiler_tests:false
          ~f:(fun _args -> parse_backend ?fqid:sim))



let readme () : string =
  Act_utils.My_string.format_for_readme
    {|
The `backend` command group contains commands for running test backends,
as well as processing their results.
|}


let command : Command.t =
  Command.group ~summary:"commands for dealing with test backends" ~readme
    [ ("run", run_command)
    ; ("parse", parse_command) ]
