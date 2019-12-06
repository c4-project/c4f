(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core_kernel

open struct
  module Ac = Act_common
end

module Field = struct
  type t = Machine | Emits

  let alist : (string, t) List.Assoc.t =
    [("machine", Machine); ("emits", Emits)]

  let get_from_spec (field : t) ~(spec : Act_machine.Qualified.Compiler.t) :
      string =
    match field with
    | Machine ->
        spec |> Act_machine.Qualified.m_spec_id |> Ac.Id.to_string
    | Emits ->
        spec |> Act_machine.Qualified.spec_without_id
        |> Act_compiler.Spec.emits |> Ac.Id.to_string
end

let run (_standard_args : Common_cmd.Args.Standard.t) (o : Ac.Output.t)
    (cfg : Act_config.Global.t) ~(compiler_id : Ac.Id.t)
    ~(fields : Field.t list) : unit Or_error.t =
  if List.is_empty fields then
    Ac.Output.pw o "No fields were supplied after the compiler ID.@." ;
  Or_error.Let_syntax.(
    let%map spec =
      Common_cmd.Language_support.Lookup.lookup_in_cfg ~cfg compiler_id
    in
    List.iter fields ~f:(fun field ->
        field |> Field.get_from_spec ~spec |> Stdio.print_endline))

let readme () : string =
  Act_utils.My_string.format_for_readme
    {| Queries the ACT configuration for information about a single compiler.

       This command takes a compiler ID, followed by a list of field flag names.
       It then, for each field flag in order, outputs the corresponding
    field from the identified compiler's spec on a new line.
|}

let command : Command.t =
  Command.basic ~summary:"outputs information about a single compiler"
    Command.Let_syntax.(
      let%map_open standard_args = Common_cmd.Args.Standard.get
      and compiler_id, fields =
        anon
          (t2
             ("COMPILER_ID" %: Common_cmd.Args.id_type)
             (sequence
                ("FIELD" %: Command.Arg_type.of_alist_exn Field.alist)))
      in
      fun () ->
        Common_cmd.Common.lift_command standard_args
          ~f:(run standard_args ~compiler_id ~fields))
