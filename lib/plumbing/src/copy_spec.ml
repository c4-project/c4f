(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core (* not Base or Core_kernel; for Sys *)

module Tx = Travesty_base_exts

type 'path t = Directory of 'path | Files of 'path list | Nothing
[@@deriving variants]

let file (path : 'a) : 'a t = Files [path]

let map (spec : 'a t) ~(f : 'a -> 'b) : 'b t =
  match spec with
  | Directory d ->
      Directory (f d)
  | Files fs ->
      Files (List.map ~f fs)
  | Nothing ->
      Nothing

let get_file : string t -> string Or_error.t = function
  | Files fs ->
      Tx.List.one fs
  | Directory _ ->
      Or_error.error_string "Expected one file; got directory"
  | Nothing ->
      Or_error.error_string "Expected one file; got nothing"

let validate_dir (dir_f : Fpath.t) : unit Or_error.t =
  let dir = Fpath.to_string dir_f in
  match Sys.is_directory dir with
  | `Yes | `Unknown ->
      Ok ()
  | `No ->
      Or_error.error_s [%message "Expected this path to be a directory" ~dir]

let validate_file (file_f : Fpath.t) : unit Or_error.t =
  let file = Fpath.to_string file_f in
  match Sys.is_file file with
  | `Yes | `Unknown ->
      Ok ()
  | `No ->
      Or_error.error_s [%message "Expected this path to be a file" ~file]

let validate_files (files : Fpath.t list) : unit Or_error.t =
  files |> List.map ~f:validate_file |> Or_error.combine_errors_unit

let validate_local : Fpath.t t -> unit Or_error.t = function
  | Directory dir ->
      validate_dir dir
  | Files files ->
      validate_files files
  | Nothing ->
      Ok ()

module Pair = struct
  type nonrec 'a t = {input: 'a t; output: 'a t}

  let map (pair : 'a t) ~(f : 'a -> 'b) : 'b t =
    {input= map ~f pair.input; output= map ~f pair.output}
end
