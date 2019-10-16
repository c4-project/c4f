(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core

let make_changes_list ?(set_name : string option)
    ?(set_postcondition : string option) () :
    Act_c_mini.Constant.t Act_litmus.Header.Change.t list Or_error.t =
  ignore set_name ;
  ignore set_postcondition ;
  Or_error.unimplemented "TODO"

let run ?(set_name : string option) ?(set_postcondition : string option)
    (_args : Common_cmd.Args.Standard.t Common_cmd.Args.With_files.t) _o
    _cfg : unit Or_error.t =
  Or_error.Let_syntax.(
    let%bind _changes = make_changes_list ?set_name ?set_postcondition () in
    Or_error.unimplemented "TODO")

let readme () : string =
  Act_utils.My_string.format_for_readme
    {|
Applies patches to the header content in a C litmus test.
|}

let command : Command.t =
  Command.basic ~summary:"replaces header of a C litmus test" ~readme
    Command.Let_syntax.(
      let%map_open standard_args =
        Common_cmd.Args.(With_files.get Standard.get)
      and set_postcondition =
        flag "postcondition" (optional string)
          ~doc:"Replaces the postcondition of the test."
      and set_name =
        flag "name" (optional string) ~doc:"Replaces the name of the test."
      in
      fun () ->
        Common_cmd.Common.lift_command
          (Common_cmd.Args.With_files.rest standard_args)
          ~f:(run standard_args ?set_name ?set_postcondition))
