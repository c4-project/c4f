open Core
open Rresult

type t =
  { c_path     : string
  ; litc_path  : string
  ; out_root   : string
  ; a_paths    : (string, string) List.Assoc.t
  ; lita_paths : (string, string) List.Assoc.t
  }

type ent_type =
  | File
  | Dir
  | Nothing
  | Unknown

let get_ent_type (path : string) : ent_type =
  match Sys.file_exists path with
  | `No -> Nothing
  | `Unknown -> Unknown
  | `Yes ->
     match Sys.is_directory path with
     | `No -> File
     | `Unknown -> Unknown
     | `Yes -> Dir

(** [mkdir path] tries to make a directory at path [path].
    If [path] exists and is a directory, it does nothing.
    If [path] exists but is a file, or another error occurred, it returns
    an error message. *)
let mkdir (path : string) =
  match get_ent_type path with
  | Dir -> R.ok ()
  | File -> R.error_msgf "%s exists, but is a file" path
  | Unknown -> R.error_msgf "couldn't determine whether %s already exists" path
  | Nothing ->
     try
       Unix.mkdir path;
       R.ok ()
     with
     | Unix.Unix_error (errno, func, arg) ->
        R.error_msgf "couldn't %s %s: %s"
                     func arg (Unix.Error.message errno)

let c_path_of results_path : string -> string =
  Filename.concat (Filename.concat results_path "C")
let litc_path_of results_path : string -> string =
  Filename.concat (Filename.concat results_path "litmus")

let a_dir_of (root : string) (cname : string) : string =
  Filename.concat root (cname ^ "_asm")

let a_path_of (root : string) (file : string) (cname : string) : string =
  Filename.concat (a_dir_of root cname) file

let lita_dir_of (root : string) (cname : string) : string =
  Filename.concat root (cname ^ "_litmus")

let lita_path_of (root : string) (file : string) (cname : string) : string =
  Filename.concat (lita_dir_of root cname) file

let make_dir_structure ps =
  try
    if Sys.is_directory_exn ps.out_root
    then Utils.iter_result
           mkdir
           (List.map ~f:(fun (c, _) -> a_dir_of ps.out_root c) ps.a_paths
            @ List.map ~f:(fun (c, _) -> lita_dir_of ps.out_root c) ps.lita_paths)
    else R.error_msgf "%s not a directory" ps.out_root
  with
  | Sys_error e -> R.error_msgf "system error while making directories: %s" e

let make specs ~root_path ~results_path ~c_fname =
  let basename   = Filename.basename (Filename.chop_extension c_fname) in
  let lit_fname  = basename ^ ".litmus" in
  let spec_map f = List.map ~f:(fun (c, _) -> (c, f c)) specs in
  let asm_fname  = basename ^ ".s" in
  { out_root     = root_path
  ; c_path       = c_path_of    results_path c_fname
  ; litc_path    = litc_path_of results_path lit_fname
  ; a_paths      = spec_map (a_path_of root_path asm_fname)
  ; lita_paths   = spec_map (lita_path_of root_path lit_fname)
  }
