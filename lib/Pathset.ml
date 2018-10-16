open Core
open Utils
open Utils.MyContainers

type compiler =
  { asm_path  : string
  ; lita_path : string
  ; herd_path : string
  }

type t =
  { basename       : string
  ; c_path         : string
  ; litc_path      : string
  ; out_root       : string
  ; compiler_paths : (CompilerSpec.Id.t, compiler) List.Assoc.t
  }

let compiler_paths_of ps cid =
  List.Assoc.find_exn
    ps.compiler_paths
    cid
    ~equal:(CompilerSpec.Id.equal)
;;

let compiler_asm_path ps cid =
  (compiler_paths_of ps cid).asm_path
;;

let compiler_lita_path ps cid =
  (compiler_paths_of ps cid).lita_path
;;

let compiler_herd_path ps cid =
  (compiler_paths_of ps cid).herd_path
;;

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
  let open Or_error in
  match get_ent_type path with
  | Dir -> Result.ok_unit
  | File -> error "path exists, but is a file" path [%sexp_of: string]
  | Unknown -> error "couldn't determine whether path already exists" path [%sexp_of: string]
  | Nothing -> Or_error.try_with (fun () -> Unix.mkdir path)

let subpaths (path : string list) : string list =
  List.map ~f:MyFilename.concat_list (MyList.prefixes path)

let mkdir_p (path : string list) =
  Or_error.all_unit (List.map ~f:mkdir (subpaths path))

let c_path_of results_path name =
  [results_path; "C"; name]

let litc_path_of results_path name =
  [results_path; "litmus"; name]

let asm_dir_of (root : string) (cid : string list) : string list =
  [root] @ cid @ ["asm"]

let asm_path_of (root : string) (file : string) (cid : string list) : string list =
  asm_dir_of root cid @ [file]

let lita_dir_of (root : string) (cid : string list) : string list =
  [root] @ cid @ ["litmus"]

let lita_path_of (root : string) (file : string) (cid : string list) : string  list =
  lita_dir_of root cid @ [file]

let herd_dir_of (root : string) (cid : string list) : string list =
  [root] @ cid @ ["herd"]

let herd_path_of (root : string) (file : string) (cid : string list) : string  list =
  herd_dir_of root cid @ [file]

let mkdirs ps =
  Or_error.(
    tag ~tag:"Couldn't make directories"
      (try_with_join
         ( fun () ->
             if Sys.is_directory_exn ps.out_root
             then MyList.iter_result
                 mkdir_p
                 (List.concat_map
                    ~f:(fun (c, _) ->
                        [ asm_dir_of ps.out_root c
                        ; lita_dir_of ps.out_root c
                        ; herd_dir_of ps.out_root c
                        ])
                    ps.compiler_paths
                 )
             else error "not a directory" ps.out_root [%sexp_of: string]
         )
      )
  )

let lcat = MyFilename.concat_list

let make_compiler root_path basename cid =
  let asm_fname  = basename ^ ".s" in
  let lita_fname = basename ^ ".s.litmus" in
  let herd_fname = basename ^ ".herd.txt" in
  { asm_path  = lcat (asm_path_of  root_path asm_fname cid)
  ; lita_path = lcat (lita_path_of root_path lita_fname cid)
  ; herd_path = lcat (herd_path_of root_path herd_fname cid)
  }
;;

let make specs ~in_root ~out_root ~c_fname =
  let basename   = Filename.basename (Filename.chop_extension c_fname) in
  let spec_map f = CompilerSpec.Set.map ~f:(fun c _ -> (c, f c)) specs in
  let lit_fname  = basename ^ ".litmus" in
  { basename
  ; out_root
  ; c_path         = lcat (c_path_of    in_root c_fname)
  ; litc_path      = lcat (litc_path_of in_root lit_fname)
  ; compiler_paths = spec_map (make_compiler out_root basename)
  }
;;

let make_and_mkdirs specs ~in_root ~out_root ~c_fname =
  let paths = make specs ~in_root ~out_root ~c_fname in
  Or_error.(mkdirs paths >>= (fun _ -> return paths))

let pp f ps =
  Format.pp_open_vbox f 4;
  Format.fprintf f "@[Paths for '%s'@ --@]" ps.basename;

  let p dir (k, v) =
    Format.pp_print_cut f ();
    MyFormat.pp_kv f (sprintf "%s (%s)" k dir) String.pp v
  in

  let in_paths =
    [ "C", ps.c_path
    ; "C/litmus", ps.litc_path
    ]
  in
  List.iter in_paths ~f:(p "in");

  let out_paths =
    List.concat_map
      ~f:(
        fun (c, {asm_path; lita_path; herd_path}) ->
          [ CompilerSpec.Id.to_string c, asm_path
          ; (CompilerSpec.Id.to_string c) ^ "/litmus", lita_path
          ; (CompilerSpec.Id.to_string c) ^ "/herd", herd_path
          ]
      )
      ps.compiler_paths
  in
  List.iter out_paths ~f:(p "out");

  Format.pp_close_box f ()

