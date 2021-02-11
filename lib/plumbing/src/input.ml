(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Stdio

type t = File of Fpath.t | Stdin of {file_type: string option}
[@@deriving variants]

(* overrides to lift options into optional arguments *)
let stdin ?file_type () : t = stdin ~file_type

let file_type : t -> string option = function
  | File fp ->
      Option.some_if (Fpath.exists_ext fp)
        (String.lstrip ~drop:(Char.equal '.') (Fpath.get_ext fp))
  | Stdin sd -> sd.file_type

include Io_common.Make (struct
  type nonrec t = t

  let of_fpath : Fpath.t -> t = file

  let to_fpath_opt : t -> Fpath.t option = function
    | File f -> Some f
    | Stdin _ -> None

  let std () = stdin ()

  let std_name = "stdin"
end)

let with_input (src : t) ~(f : Stdio.In_channel.t -> 'a Or_error.t) :
    'a Or_error.t =
  Or_error.(
    match src with
    | File file ->
        let s = Fpath.to_string file in
        tag_arg
          (try_with_join (fun _ -> In_channel.with_file s ~f))
          "While reading from file:" s [%sexp_of: string]
    | Stdin _ ->
        tag ~tag:"While reading from standard input:"
          (try_with_join (fun _ -> f In_channel.stdin)))
