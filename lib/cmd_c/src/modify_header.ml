(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core

let run ?(name : [< `Keep | `Replace_with of string] option)
    ?(postcondition : [< `Keep | `Clear | `Replace_with of string] option)
    (args : Common_cmd.Args.Standard.t Common_cmd.Args.With_files.t) _o _cfg
    : unit Or_error.t =
  Or_error.Let_syntax.(
    let%bind changes =
      Act_c_mini.Litmus_header.Change_set.of_args ?name ?postcondition ()
    in
    Common_cmd.Args.With_files.run_filter
      (module Act_c_mini.Litmus_header.Filters.Modify)
      args ~aux_in:changes)

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
      and postcondition =
        choose_one
          [ flag "no-postcondition" no_arg
              ~doc:"Removes any postcondition present."
            |> map ~f:(fun present -> Option.some_if present `Clear)
          ; flag "postcondition" (optional string)
              ~doc:"POST Replaces the postcondition of the test."
            |> map ~f:(Option.map ~f:(fun (x : string) -> `Replace_with x))
          ]
          ~if_nothing_chosen:(`Default_to `Keep)
      and name =
        flag "name" (optional string)
          ~doc:"NAME Replaces the name of the test."
        |> map ~f:(Option.map ~f:(fun n -> `Replace_with n))
      in
      fun () ->
        Common_cmd.Common.lift_command
          (Common_cmd.Args.With_files.rest standard_args)
          ~f:(run standard_args ?name ~postcondition))