open Core
open Utils

module M = struct
  type t =
    { c_path    : string list
    ; litc_path : string list
    ; asm_path  : string list
    ; lita_path : string list
    ; herd_path : string list
    } [@@deriving fields]
end
include M

module File = struct
  type ps = t

  type t =
    { basename   : string
    ; c_path     : string
    ; litc_path  : string
    ; asm_path   : string
    ; lita_path  : string
    ; herdc_path : string
    ; herda_path : string
    } [@@deriving fields]

  let make ps basename =
    let lcat = MyFilename.concat_list in
    { basename
    ; c_path     = lcat (M.c_path    ps @ [basename ^ ".c"])
    ; litc_path  = lcat (M.litc_path ps @ [basename ^ ".litmus"])
    ; asm_path   = lcat (M.asm_path  ps @ [basename ^ ".s"])
    ; lita_path  = lcat (M.lita_path ps @ [basename ^ ".s.litmus"])
    ; herdc_path = lcat (M.herd_path ps @ [basename ^ ".herd.txt"])
    ; herda_path = lcat (M.herd_path ps @ [basename ^ ".s.herd.txt"])
    }
  ;;
end


type ent_type =
  | File
  | Dir
  | Nothing
  | Unknown
;;

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
  | File -> error_s [%message "path exists, but is a file" ~path]
  | Unknown -> error_s [%message "couldn't determine whether path already exists" ~path]
  | Nothing -> Or_error.try_with (fun () -> Unix.mkdir path)
;;

let subpaths (path : string list) : string list =
  List.map ~f:MyFilename.concat_list (My_list.prefixes path)
;;

let mkdir_p (path : string list) =
  Or_error.all_unit (List.map ~f:mkdir (subpaths path))
;;

let all_paths (ps : t) : (string, string list) List.Assoc.t =
  let to_pair fld = (Field.name fld, Field.get fld ps) in
  Fields.to_list
    ~c_path:to_pair
    ~litc_path:to_pair
    ~asm_path:to_pair
    ~lita_path:to_pair
    ~herd_path:to_pair
;;

let mkdirs ps =
  Or_error.(
    tag ~tag:"Couldn't make directories"
      (try_with_join
         ( fun () ->
             Or_error.all_unit
               (List.map ~f:(Fn.compose mkdir_p snd) (all_paths ps))
         )
      )
  )
;;

let make id ~in_root ~out_root =
  { c_path    = [in_root; "C"]
  ; litc_path = [in_root; "Litmus"]
  ; asm_path  = [out_root] @ (Compiler.Id.to_string_list id) @ ["asm"]
  ; lita_path = [out_root] @ (Compiler.Id.to_string_list id) @ ["litmus"]
  ; herd_path = [out_root] @ (Compiler.Id.to_string_list id) @ ["herd"]
  }
;;

let%expect_test "all_paths of make" =
  let id = Compiler.Id.of_string "foo.bar.baz" in
  let ps = make id ~in_root:"inputs" ~out_root:"outputs" in
  Format.printf "@[%a@]@."
    Sexp.pp_hum
    [%sexp (all_paths ps : (string, string list) List.Assoc.t)];
  [%expect {|
    ((c_path (inputs C)) (litc_path (inputs Litmus))
     (asm_path (outputs foo bar baz asm))
     (lita_path (outputs foo bar baz litmus))
     (herd_path (outputs foo bar baz herd))) |}]
;;

let make_and_mkdirs id ~in_root ~out_root =
  let paths = make id ~in_root ~out_root in
  Or_error.(mkdirs paths >>= (fun _ -> return paths))
;;

let pp f ps =
  Format.pp_open_vbox f 0;

  let p f (k, v) =
    MyFormat.pp_kv f k String.pp (MyFilename.concat_list v)
  in
  Format.pp_print_list
    ~pp_sep:Format.pp_print_cut
    p
    f
    (all_paths ps);

  Format.pp_close_box f ()
;;
