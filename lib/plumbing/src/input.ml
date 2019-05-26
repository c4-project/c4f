(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

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
  | Stdin sd ->
      sd.file_type

let to_string : t -> string = function
  | File s ->
      Fpath.to_string s
  | Stdin _ ->
      "(stdin)"

let pp : t Fmt.t = Fmt.of_to_string to_string

let of_fpath : Fpath.t -> t = file

let of_fpath_opt : Fpath.t option -> t =
  Option.value_map ~f:file ~default:(stdin ())

let of_string_opt : string option -> t Or_error.t =
  Fpath_helpers.lift_str ~f:file ~default:(stdin ())

let to_file : t -> Fpath.t option = function
  | File f ->
      Some f
  | Stdin _ ->
      None

let to_file_err (src : t) : Fpath.t Or_error.t =
  Result.of_option (to_file src)
    ~error:(Error.createf "Must read from a file, got %s" (to_string src))

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
