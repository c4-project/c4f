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

open Core
module Pb = Plumbing
module Tx = Travesty_base_exts
include Fs_intf

let default_sort_compare : Fpath.t -> Fpath.t -> int =
  Tx.Fn.on Fpath.to_string ~f:String_extended.collate

let filter_files ?(ext : string option) (flist : Fpath.t list) :
    Fpath.t list =
  Option.value_map ~default:flist
    ~f:(fun ext -> List.filter ~f:(Fpath.has_ext ext) flist)
    ext

let subpaths (path : Fpath.t) : Fpath.t list =
  let rec mu (prefixes : Fpath.t list) (path : Fpath.t) : Fpath.t list =
    if
      Tx.List.any path
        ~predicates:
          [Fpath.is_current_dir; Fpath.is_parent_dir; Fpath.is_root]
    then prefixes
    else
      let parent = Fpath.parent path in
      mu (parent :: prefixes) parent
  in
  let npath = Fpath.normalize path in
  mu [npath] npath

module Unix : S = struct
  type ent_type = File | Dir | Nothing | Unknown

  let get_ent_type (path : string) : ent_type =
    match Sys.file_exists path with
    | `No ->
        Nothing
    | `Unknown ->
        Unknown
    | `Yes -> (
      match Sys.is_directory path with
      | `No ->
          File
      | `Unknown ->
          Unknown
      | `Yes ->
          Dir )

  let path_absent_error (path : Fpath.t) : unit Or_error.t =
    Or_error.error_s
      [%message "path doesn't exist" ~path:(Fpath.to_string path)]

  let path_indeterminate_error (path : string) : unit Or_error.t =
    Or_error.error_s
      [%message "couldn't determine whether path already exists" ~path]

  let check ?(on_absent : Fpath.t -> unit Or_error.t = path_absent_error)
      (path : Fpath.t) ~(on_dir : Fpath.t -> unit Or_error.t)
      ~(on_file : Fpath.t -> unit Or_error.t) : unit Or_error.t =
    let path_s = Fpath.to_string path in
    match get_ent_type path_s with
    | Dir ->
        on_dir path
    | File ->
        on_file path
    | Unknown ->
        path_indeterminate_error path_s
    | Nothing ->
        on_absent path

  let check_is_dir ?(on_absent : (Fpath.t -> unit Or_error.t) option) :
      Fpath.t -> unit Or_error.t =
    check ?on_absent ~on_dir:(Fn.const Result.ok_unit) ~on_file:(fun path ->
        Or_error.error_s
          [%message
            "path exists, but is a file" ~path:(Fpath.to_string path)] )

  let check_is_file ?(on_absent : (Fpath.t -> unit Or_error.t) option) :
      Fpath.t -> unit Or_error.t =
    check ?on_absent ~on_file:(Fn.const Result.ok_unit) ~on_dir:(fun path ->
        Or_error.error_s
          [%message
            "path exists, but is a directory" ~path:(Fpath.to_string path)]
    )

  let actually_mkdir (path : Fpath.t) : unit Or_error.t =
    let path_s = Fpath.to_string path in
    Or_error.try_with (fun () -> Unix.mkdir path_s)

  let mkdir : Fpath.t -> unit Or_error.t =
    check_is_dir ~on_absent:actually_mkdir

  let mkdir_p (path : Fpath.t) =
    Or_error.all_unit (List.map ~f:mkdir (subpaths path))

  let readdir path =
    Or_error.(
      tag_arg
        (try_with (fun () -> Sys.readdir path))
        "Couldn't read directory" path [%sexp_of: string])

  let map_combine (xs : 'a list) ~(f : 'a -> 'b Or_error.t) :
      'b list Or_error.t =
    Or_error.combine_errors (List.map ~f xs)

  let get_files ?(compare = default_sort_compare) ?ext (path : Fpath.t) =
    let open Or_error.Let_syntax in
    let%bind file_str_array = readdir (Fpath.to_string path) in
    let file_strs = Array.to_list file_str_array in
    let%map files = map_combine ~f:Pb.Fpath_helpers.of_string file_strs in
    let with_ext = filter_files ?ext files in
    List.sort ~compare with_ext
end
