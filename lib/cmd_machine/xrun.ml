(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core

open struct
  module Ac = Act_common
  module Pb = Plumbing
end

let machine_id_or_default (_cfg : Act_config.Global.t)
    (machine : Ac.Id.t option) : Ac.Id.t Or_error.t =
  match machine with
  | Some id ->
      Or_error.return id
  | None ->
      (* TODO(@MattWindsor91): default machine! *)
      Or_error.unimplemented "default machine IDs"

let machine_runner ?(machine : Ac.Id.t option) (cfg : Act_config.Global.t) :
    (module Pb.Runner_types.S) Or_error.t =
  (* TODO(@MattWindsor91): a lot of this should be decoupled in to a helper
     module. *)
  let machine_specs = Act_config.Global.machines cfg in
  Or_error.Let_syntax.(
    let%bind id = machine_id_or_default cfg machine in
    let%map spec = Ac.Spec.Set.get ~id_type:"machine" machine_specs ~id in
    Act_machine.Spec.runner spec)

let argv_f (_spec : Pb.Copy_projection.t Pb.Copy_spec.Pair.t) :
    string list Or_error.t =
  (* TODO(@MattWindsor91): support argvs, ideally constructed with both
     working set and non-working set components *)
  Or_error.return []

let make_copy_specs (prog : string) : Fpath.t Pb.Copy_spec.Pair.t Or_error.t
    =
  Or_error.Let_syntax.(
    let%map prog_file = Pb.Fpath_helpers.of_string prog in
    let input = Pb.Copy_spec.file prog_file in
    let output = Pb.Copy_spec.nothing in
    Pb.Copy_spec.Pair.{input; output})

let run ?(machine : Ac.Id.t option)
    (_standard_args : Common_cmd.Args.Standard.t) (_o : Ac.Output.t)
    (cfg : Act_config.Global.t) ~(prog : string) : unit Or_error.t =
  Or_error.Let_syntax.(
    let%bind (module Runner) = machine_runner ?machine cfg in
    let%bind cs_pair = make_copy_specs prog in
    Runner.run_with_copy cs_pair ~oc:Stdio.stdout ~prog_f:Pb.Runner.copy_prog
      ~argv_f ~prog)

let readme () : string =
  Act_utils.My_string.format_for_readme
    {|
`xrun` (cross-run) supports the running of binaries located on a local machine
on a machine that may or may not be local.

This command has four limitations.  First, the binary must exist on the local
machine.  Second, it must be ABI-compatible
with the target machine (xrun doesn't do any form of translation, emulation,
or anything sophisticated -- it just copies the binary).  Third, the binary
cannot take any arguments.  Last, no working set of input and output files may
be copied.

The last two points are a limitation of xrun and may be improved in later
revisions.
    |}

let command : Command.t =
  Command.basic ~summary:"run a local binary on a machine" ~readme
    Command.Let_syntax.(
      let%map_open standard_args = Common_cmd.Args.Standard.get
      and machine =
        flag "-machine"
          (optional Common_cmd.Args.id_type)
          ~doc:"MACHINE_ID the machine on which the command should be run"
      and prog = anon ("PROG_FILE" %: Filename.arg_type) in
      fun () ->
        Common_cmd.Common.lift_command standard_args
          ~f:(run standard_args ?machine ~prog))
