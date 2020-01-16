(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core_kernel

(* May be useful elsewhere someday. *)
let ssh_type = Command.Arg_type.create Plumbing.Ssh_runner.Config.of_string

let name_of_ssh (ssh : Plumbing.Ssh_runner.Config.t option) : Act_common.Id.t
    =
  ssh
  |> Option.value_map ~f:Plumbing.Ssh_runner.Config.host ~default:"localhost"
  |> Act_common.Id.of_string

let output_spec ?(ssh : Plumbing.Ssh_runner.Config.t option)
    (spec : Act_machine.Spec.t) : unit =
  let name = name_of_ssh ssh in
  let map = Map.singleton (module Act_common.Id) name spec in
  let specs = Act_common.Spec.Set.of_map map in
  Fmt.pr "@[<v>%a@]@." Act_config.Reify.Machines.pp specs

let run ?(ssh : Plumbing.Ssh_runner.Config.t option)
    (_o : Act_common.Output.t) (_cfg : Act_config.Global.t) : unit Or_error.t
    =
  let via =
    Option.value_map ssh ~f:Act_machine.Via.ssh
      ~default:Act_machine.Via.local
  in
  Or_error.Let_syntax.(
    let%map mspec =
      Act_machine.Probe.probe via
        ~backend_styles:Common_cmd.Backend_support.style_modules
        ~compiler_styles:Common_cmd.Language_support.style_modules
    in
    output_spec ?ssh mspec)

let readme () : string =
  Act_utils.My_string.format_for_readme
    {| Probes a machine (locally by default; optionally a remote machine via
    SSH) for compilers and backends.  If successful, the command prints a
    configuration stanza for the machine to stdout. |}

let command : Core_kernel.Command.t =
  Command.basic ~summary:"probes a machine for ACT configuration" ~readme
    Command.Let_syntax.(
      let%map_open standard_args = Common_cmd.Args.Standard.get
      and ssh =
        flag "-ssh" (optional ssh_type)
          ~doc:"SSH_SPEC host@port:directory if probing remotely"
      in
      fun () -> Common_cmd.Common.lift_command standard_args ~f:(run ?ssh))
