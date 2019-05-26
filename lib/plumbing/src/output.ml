(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to permit
   persons to whom the Software is furnished to do so, subject to the
   following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
   NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
   DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
   OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
   USE OR OTHER DEALINGS IN THE SOFTWARE. *)

open Core
open Stdio

type t = File of Fpath.t | Stdout [@@deriving variants]

let temp ~(prefix : string) ~(ext : string) : t =
  file (Fpath.v (Filename.temp_file prefix ("." ^ ext)))

let to_string : t -> string = function
  | File s ->
      Fpath.to_string s
  | Stdout ->
      "(stdout)"

let as_input : t -> Input.t Or_error.t = function
  | File f ->
      Or_error.return (Input.file f)
  | x ->
      Or_error.errorf "Can't use %s as an input source" (to_string x)

let pp : t Fmt.t = Fmt.of_to_string to_string

let of_fpath : Fpath.t -> t = file

let of_fpath_opt : Fpath.t option -> t =
  Option.value_map ~f:file ~default:stdout

let of_string_opt : string option -> t Or_error.t =
  Fpath_helpers.lift_str ~f:of_fpath ~default:stdout

let to_file : t -> Fpath.t option = function
  | File f ->
      Some f
  | Stdout ->
      None

let to_file_err (src : t) : Fpath.t Or_error.t =
  Result.of_option (to_file src)
    ~error:(Error.createf "Must write to a file, got %s" (to_string src))

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
