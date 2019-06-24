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
module M_spec = Act_machine.Spec

let run_sim ?(arch = Act_sim.Arch.C) ?(fqid : Id.t = Id.of_string "herd")
    (_o : Output.t) (cfg : Act_config.Act.t) : unit Or_error.t =
  let module Res = Sim_support.Make_resolver (struct
    let cfg = cfg
  end) in
  Or_error.Let_syntax.(
    (* TODO(@MattWindsor91): use argv, etc. *)
    let%bind (module Sim) = Res.resolve_single fqid in
    Sim.Filter.run arch (Plumbing.Input.stdin ()) Plumbing.Output.stdout
    (*Sim.run_direct ?arch cfg argv*))

let command : Command.t =
  Command.basic ~summary:"runs a configured test backend"
    Command.Let_syntax.(
      let%map_open standard_args = Args.Standard.get
      and sim = Args.simulator ()
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
        (* and argv = flag "--" Command.Flag.escape ~doc:"STRINGS Arguments
           to send to Herd directly." *)
      in
      fun () ->
        Common.lift_command standard_args ~with_compiler_tests:false
          ~f:(fun _args -> run_sim ?arch (* ?argv *) ?fqid:sim))
