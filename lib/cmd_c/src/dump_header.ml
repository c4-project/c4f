(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core

let run (args : Common_cmd.Args.Standard.t Common_cmd.Args.With_files.t) _o :
    unit Or_error.t =
  Common_cmd.Args.With_files.run_filter Act_litmus_c.Header.Filters.run_dump
    args

let readme () : string =
  Act_utils.My_string.format_for_readme
    {|
Dumps the header content of a C litmus test into a JSON file.

The format of this JSON file is essentially that of the 'litmus_header'
segment of a Delitmus aux file.
|}

let command : Command.t =
  Command.basic ~summary:"dumps header of a C litmus test" ~readme
    Command.Let_syntax.(
      let%map_open standard_args =
        Common_cmd.Args.(With_files.get Standard.get)
      in
      fun () ->
        Common_cmd.Args.Standard.lift_command
          (Common_cmd.Args.With_files.rest standard_args)
          ~f:(run standard_args))
