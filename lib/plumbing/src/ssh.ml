(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core

type t = {user: string option [@sexp.option]; host: string}
[@@deriving sexp, equal, fields, make]

let to_string (ssh : t) : string =
  Option.value_map (user ssh) ~default:(host ssh) ~f:(fun u ->
      sprintf "%s@%s" u (host ssh))

let pp : t Fmt.t = Fmt.of_to_string to_string

let run_inner ?(user : string option) ~(host : string) ~(prog : string)
    ~(argv : string list) : string list Or_error.t =
  Or_error.(
    tag_arg
      (try_with (fun () ->
           Shell.ssh_lines "%s %s" prog
             (String.concat ~sep:" " argv)
             ~host ?user))
      "Error running remote command via ssh:"
      (host, Option.value ~default:"(default user)" user)
      [%sexp_of: string * string])

let run ?(oc : Out_channel.t option) (ssh : t) ~(prog : string)
    ~(argv : string list) : unit Or_error.t =
  let user = user ssh in
  let host = host ssh in
  Or_error.(
    run_inner ?user ~host ~prog ~argv
    >>| fun output ->
    Option.iter ~f:(fun o -> Out_channel.output_lines o output) oc)
