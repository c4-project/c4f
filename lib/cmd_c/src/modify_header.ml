(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core

let run ?(name : [< `Keep | `Replace_with of string] option)
    ?(postcondition : [< `Keep | `Clear | `Replace_with of string] option)
    (args : Common_cmd.Args.Standard.t Common_cmd.Args.With_files.t) _o :
    unit Or_error.t =
  Or_error.Let_syntax.(
    let%bind changes =
      C4f_litmus_c.Header.Change_set.of_args ?name ?postcondition ()
    in
    Common_cmd.Args.With_files.run_filter
      (C4f_litmus_c.Header.Filters.run_modify ~changes)
      args )

let readme () : string =
  C4f_utils.My_string.format_for_readme
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
              ~doc:"removes any postcondition present"
            |> map ~f:(fun present -> Option.some_if present `Clear)
          ; flag "postcondition" (optional string)
              ~doc:"POST replaces the postcondition of the test"
            |> map ~f:(Option.map ~f:(fun (x : string) -> `Replace_with x))
          ]
          ~if_nothing_chosen:(Default_to `Keep)
      and name =
        flag "name" (optional string)
          ~doc:"NAME replaces the name of the test"
        |> map ~f:(Option.map ~f:(fun n -> `Replace_with n))
      in
      fun () ->
        Common_cmd.Args.Standard.lift_command
          (Common_cmd.Args.With_files.rest standard_args)
          ~f:(run standard_args ?name ~postcondition) )
