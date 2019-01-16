(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation
   files (the "Software"), to deal in the Software without
   restriction, including without limitation the rights to use, copy,
   modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

open Core

type 'path t =
  | Directory of 'path
  | Files of 'path list
  | Nothing
[@@deriving variants]
;;

let file (path : 'a) : 'a t = Files [ path ]

let map (spec : 'a t) ~(f : 'a -> 'b) : 'b t =
  match spec with
  | Directory d  -> Directory (f d)
  | Files     fs -> Files (List.map ~f fs)
  | Nothing      -> Nothing
;;

let validate_dir (dir_f : Fpath.t) : unit Or_error.t =
  let dir = Fpath.to_string dir_f in
  match Sys.is_directory dir with
  | `Yes | `Unknown -> Result.ok_unit
  | `No ->
    Or_error.error_s
      [%message "Expected this path to be a directory" ~dir]
;;

let validate_file (file_f : Fpath.t) : unit Or_error.t =
  let file = Fpath.to_string file_f in
  match Sys.is_file file with
  | `Yes | `Unknown -> Result.ok_unit
  | `No ->
    Or_error.error_s
      [%message "Expected this path to be a file" ~file]
;;

let validate_files (files : Fpath.t list) : unit Or_error.t =
  files
  |> List.map ~f:validate_file
  |> Or_error.combine_errors_unit
;;

let validate_local : Fpath.t t -> unit Or_error.t = function
  | Directory dir   -> validate_dir   dir
  | Files     files -> validate_files files
  | Nothing         -> Result.ok_unit
;;

module Pair = struct
  type nonrec 'a t =
    { input  : 'a t
    ; output : 'a t
    }
  ;;

  let map (pair : 'a t) ~(f : 'a -> 'b) : 'b t =
    { input  = map ~f pair.input
    ; output = map ~f pair.output
    }
  ;;
end
