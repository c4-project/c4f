(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core
open Stdio

type t = File of Fpath.t | Stdout [@@deriving variants]

let temp ~(prefix : string) ~(ext : string) : t =
  file (Fpath.v (Filename.temp_file prefix ("." ^ ext)))

include Io_common.Make (struct
  type nonrec t = t

  let of_fpath : Fpath.t -> t = file

  let to_fpath_opt : t -> Fpath.t option = function
    | File f ->
        Some f
    | Stdout ->
        None

  let std () = stdout

  let std_name = "stdout"
end)

let as_input : t -> Input.t Or_error.t = function
  | File f ->
      Or_error.return (Input.file f)
  | x ->
      Or_error.errorf "Can't use %s as an input source" (to_string x)

let with_file_output (fpath : Fpath.t) f =
  let fpath_raw = Fpath.to_string fpath in
  Or_error.(
    tag_arg
      (try_with_join (fun _ -> Out_channel.with_file fpath_raw ~f))
      "While writing to file:" fpath_raw [%sexp_of: string])

let with_stdout_output f =
  Or_error.try_with_join (fun _ -> f Out_channel.stdout)

let with_output (snk : t) ~(f : Out_channel.t -> 'a Or_error.t) :
    'a Or_error.t =
  ( match snk with
  | File fpath ->
      with_file_output fpath
  | Stdout ->
      with_stdout_output )
    f
