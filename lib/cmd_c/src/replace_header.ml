(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core

let run (args : Common_cmd.Args.Standard.t Common_cmd.Args.With_files.t) _o
    _cfg ~(header_file : string) : unit Or_error.t =
  Or_error.Let_syntax.(
    let%bind header_path = Plumbing.Fpath_helpers.of_string header_file in
    let%bind header = Act_c_mini.Litmus_header.load ~path:header_path in
    Common_cmd.Args.With_files.run_filter
      (module Act_c_mini.Litmus_header.Replace_filter)
      args ~aux_in:header)

let readme () : string =
  Act_utils.My_string.format_for_readme
    {|
Substitutes the contents of a JSON file for the header content
in a C litmus test.

The format of this JSON file is essentially that of the 'litmus_header'
segment of a Delitmus aux file.
|}

let command : Command.t =
  Command.basic ~summary:"replaces header of a C litmus test" ~readme
    Command.Let_syntax.(
      let%map_open standard_args =
        Common_cmd.Args.(With_files.get Standard.get)
      and header_file =
        flag "header"
          (required Filename.arg_type)
          ~doc:"FILE a file containing the new header"
      in
      fun () ->
        Common_cmd.Common.lift_command
          (Common_cmd.Args.With_files.rest standard_args)
          ~f:(run standard_args ~header_file))
