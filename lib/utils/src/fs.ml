(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core
module Pb = Plumbing
module Tx = Travesty_base_exts

let default_sort_compare : Fpath.t -> Fpath.t -> int =
  Tx.Fn.on Fpath.to_string ~f:String_extended.collate

let filter_files ?(ext : string option) (flist : Fpath.t list) : Fpath.t list
    =
  Option.value_map ~default:flist
    ~f:(fun ext -> List.filter ~f:(Fpath.has_ext ext) flist)
    ext

let subpaths (path : Fpath.t) : Fpath.t list =
  let rec mu (prefixes : Fpath.t list) (path : Fpath.t) : Fpath.t list =
    if
      Tx.List.any path
        ~predicates:[Fpath.is_current_dir; Fpath.is_parent_dir; Fpath.is_root]
    then prefixes
    else
      let parent = Fpath.parent path in
      mu (parent :: prefixes) parent
  in
  let npath = Fpath.normalize path in
  mu [npath] npath

module Unix : Fs_types.S = struct
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
    check ?on_absent ~on_dir:(Fn.const (Ok ())) ~on_file:(fun path ->
        Or_error.error_s
          [%message
            "path exists, but is a file" ~path:(Fpath.to_string path)] )

  let check_is_file ?(on_absent : (Fpath.t -> unit Or_error.t) option) :
      Fpath.t -> unit Or_error.t =
    check ?on_absent ~on_file:(Fn.const (Ok ())) ~on_dir:(fun path ->
        Or_error.error_s
          [%message
            "path exists, but is a directory" ~path:(Fpath.to_string path)] )

  let actually_mkdir (path : Fpath.t) : unit Or_error.t =
    let path_s = Fpath.to_string path in
    Or_error.try_with (fun () -> Unix.mkdir path_s)

  let mkdir : Fpath.t -> unit Or_error.t =
    check_is_dir ~on_absent:actually_mkdir

  let mkdir_p (path : Fpath.t) =
    Or_error.all_unit (List.map ~f:mkdir (subpaths path))

  let ls_raw (path : string) : string list Or_error.t =
    Or_error.(
      tag_arg
        (try_with (fun () -> Sys.ls_dir path))
        "Couldn't read directory" path [%sexp_of: string])

  let ls (path : Fpath.t) : Fpath.t list Or_error.t =
    Or_error.(
      path |> Fpath.to_string |> ls_raw
      >>= Tx.Or_error.combine_map ~f:Pb.Fpath_helpers.of_string)

  let get_files ?(compare : Fpath.t -> Fpath.t -> int = default_sort_compare)
      ?(predicate : (Fpath.t -> bool) option) (path : Fpath.t) :
      Fpath.t list Or_error.t =
    Or_error.Let_syntax.(
      let%map all = ls path in
      let matching =
        Option.value_map predicate
          ~f:(fun f -> List.filter all ~f)
          ~default:all
      in
      List.sort ~compare matching)
end
