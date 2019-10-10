(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core

let run (args : Common_cmd.Args.Standard.t Common_cmd.Args.With_files.t) _o
    _cfg ~(aux_file : string) : unit Or_error.t =
  ignore args ;
  ignore aux_file ;
  Or_error.unimplemented "TODO"

let readme () : string =
  Act_utils.My_string.format_for_readme
    {|
Substitutes the contents of an aux JSON file for the auxiliary information
in a C litmus test.
|}

let command : Command.t =
  Command.basic ~summary:"replaces aux info in a C litmus test" ~readme
    Command.Let_syntax.(
      let%map_open standard_args =
        Common_cmd.Args.(With_files.get Standard.get)
      and aux_file =
        flag "aux"
          (required Filename.arg_type)
          ~doc:"FILE a file containing the new auxiliary litmus information"
      in
      fun () ->
        Common_cmd.Common.lift_command
          (Common_cmd.Args.With_files.rest standard_args)
          ~f:(run standard_args ~aux_file))
