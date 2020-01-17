(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core_kernel

open struct
  module Ac = Act_common
end

(* TODO(@MattWindsor91): move this or make it irrelevant somehow? *)
let is_id (id : Ac.Id.t) : Ac.Id.Property.t =
  Ac.Id.(id |> to_string |> Property.is)

let predicate_of_machine_id (machine_id : Act_common.Id.t) :
    Act_machine.Property.t Blang.t =
  machine_id |> is_id |> Act_machine.Property.id |> Blang.base

let predicate_of_style_id (style : Act_common.Id.t) :
    Act_backend.Property.t Blang.t =
  style |> is_id |> Act_backend.Property.style |> Blang.base

let find_in_listing
    (lut : Act_machine.Qualified.Backend.t Act_machine.Lookup_listing.t) :
    Act_common.Id.t option =
  (* We're not doing any fancy backend default stuff yet, so we'll just
     literally try to take the first backend that resolves. *)
  Option.(
    lut |> Act_machine.Lookup_listing.enabled |> Ac.Spec.Set.to_list
    |> List.hd >>| Ac.Spec.With_id.id)

let find_on_machine (machine_id : Act_common.Id.t)
    ~(machine_specs : Act_machine.Spec.t Ac.Spec.Set.t)
    ~(style : Act_common.Id.t) ~(with_backend_tests : bool) :
    Act_common.Id.t option =
  let machine_predicate = predicate_of_machine_id machine_id in
  let backend_predicate = predicate_of_style_id style in
  let backends =
    Or_error.ok
      (Common_cmd.Backend_support.Lookup.filtered_list ~machine_predicate
         ~predicate:backend_predicate ~test_specs:with_backend_tests
         machine_specs)
  in
  Option.bind backends ~f:find_in_listing

let default_machines (cfg : Act_config.Global.t) : Act_common.Id.t list =
  Act_config.(cfg |> Global.defaults |> Default.machines)

let find (machines : Act_common.Id.t list) ~(cfg : Act_config.Global.t)
    ~(style : Act_common.Id.t) ~(with_backend_tests : bool) :
    Act_common.Id.t Or_error.t =
  (* TODO(@MattWindsor91): contemplate default backends. *)
  let defaults = default_machines cfg in
  let all_machines = machines @ defaults in
  let machine_specs = Act_config.Global.machines cfg in
  all_machines
  |> List.find_map
       ~f:(find_on_machine ~machine_specs ~style ~with_backend_tests)
  |> Result.of_option ~error:(Error.of_string "No matching backends found.")

let run (_o : Ac.Output.t) (cfg : Act_config.Global.t)
    ~(machines : Act_common.Id.t list) ~(style : Act_common.Id.t)
    ~(with_backend_tests : bool) : unit Or_error.t =
  Or_error.Let_syntax.(
    let%map id = find machines ~cfg ~style ~with_backend_tests in
    Fmt.pr "@[%a@]@." Act_common.Id.pp id)

let readme () : string =
  Act_utils.My_string.format_for_readme
    {|
      Given a style identifier (for example, 'herd' or 'litmus') and zero
      or more target machines, this command tries to find the ID of a backend
      that implements that style.

      This command interacts with the default stanzas in the configuration file
      as follows:

      - It tries every default machine, in definition order, after exhausting
        the machines given on the command line.
    |}

let command : Command.t =
  Command.basic
    ~summary:"finds the most appropriate backend for a particular style"
    ~readme
    Command.Let_syntax.(
      let%map_open standard_args = Common_cmd.Args.Standard.get
      and style = anon ("STYLE_ID" %: Common_cmd.Args.id_type)
      and machines =
        anon (sequence ("MACHINE_ID" %: Common_cmd.Args.id_type))
      and with_backend_tests =
        flag "test-backends" no_arg
          ~doc:
            "If true, test each backend's presence and only print the \
             backends that pass"
      in
      fun () ->
        Common_cmd.Common.lift_command standard_args
          ~f:(run ~style ~machines ~with_backend_tests))
