(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core

let run (args : Common_cmd.Args.Standard.t Common_cmd.Args.With_files.t) _o
    ~(header_file : string) : unit Or_error.t =
  Or_error.Let_syntax.(
    let%bind header_input = Plumbing.Input.of_string header_file in
    let%bind replacement = C4f_litmus_c.Header.load header_input in
    Common_cmd.Args.With_files.run_filter
      (C4f_litmus_c.Header.Filters.run_replace ~replacement)
      args )

let readme () : string =
  C4f_utils.My_string.format_for_readme
    {|
Substitutes the contents of a JSON file for the header content
in a C litmus test.

The format of this JSON file is the same as that created by `dump-header`,
which, in turn, corresponds to the 'litmus_header'
segment of a Delitmus aux file.
|}

let command : Command.t =
  Command.basic ~summary:"replaces header of a C litmus test" ~readme
    Command.Let_syntax.(
      let%map_open standard_args =
        Common_cmd.Args.(With_files.get Standard.get)
      and header_file =
        flag "header"
          (required Filename_unix.arg_type)
          ~doc:"FILE a file containing the new header"
      in
      fun () ->
        Common_cmd.Args.Standard.lift_command
          (Common_cmd.Args.With_files.rest standard_args)
          ~f:(run standard_args ~header_file) )
