open Core
open Utils

module Input_mode = struct
  type t =
    | Memalloy of Fpath.t
    | Litmus_only of Fpath.t list
  ;;

  let validate_input_root_exists : Fpath.t Validate.check =
    Validate.of_error (Fs.Unix.check_is_dir)
  ;;

  let validate_file_exists : Fpath.t Validate.check =
    Validate.of_error (Fs.Unix.check_is_file)
  ;;

  let validate_files_exist : Fpath.t list Validate.check =
    Validate.list ~name:(Fpath.to_string) validate_file_exists
  ;;

  let validate : t Validate.check =
    function
    | Memalloy input_root -> validate_input_root_exists input_root
    | Litmus_only files -> validate_files_exist files
  ;;

  let memalloy ~(input_root:Fpath.t) : t Or_error.t =
    Validate.valid_or_error (Memalloy input_root) validate
  ;;

  let litmus_only ~(files:Fpath.t list) : t Or_error.t =
    Validate.valid_or_error (Litmus_only files) validate
  ;;

  let must_delitmusify : t -> bool = function
    | Memalloy    _ -> false
    | Litmus_only _ -> true
  ;;
end

module M = struct
  type t =
    { litc_files : Fpath.t list
    ; input_mode : Input_mode.t
    ; c_path     : Fpath.t
    ; asm_path   : Fpath.t
    ; lita_path  : Fpath.t
    ; herd_path  : Fpath.t
    } [@@deriving fields]
end
include M

module File = struct
  type ps = t

  type t =
    { name       : string
    ; c_path     : Fpath.t
    ; litc_path  : Fpath.t
    ; asm_path   : Fpath.t
    ; lita_path  : Fpath.t
    ; herdc_path : Fpath.t
    ; herda_path : Fpath.t
    } [@@deriving fields]

  let make (ps : ps) (litc_path : Fpath.t) =
    let name = Fpath.(basename (rem_ext litc_path)) in
    Fpath.
      { name
      ; litc_path  = litc_path
      ; c_path     = M.c_path ps    / name + ".c"
      ; asm_path   = M.asm_path  ps / name + ".s"
      ; lita_path  = M.lita_path ps / name + ".s.litmus"
      ; herdc_path = M.herd_path ps / name + ".herd.txt"
      ; herda_path = M.herd_path ps / name + ".s.herd.txt"
      }
  ;;
end

let to_files (ps : t) : File.t list =
  List.map ~f:(File.make ps) ps.litc_files
;;

let all_dirs (ps : t) : (string, Fpath.t) List.Assoc.t =
  let to_pair fld = Some ( Field.name fld, Field.get fld ps ) in
  List.filter_opt
  ( Fields.to_list
      ~input_mode:(Fn.const None)
      ~litc_files:(Fn.const None)
      ~c_path:to_pair
      ~asm_path:to_pair
      ~lita_path:to_pair
      ~herd_path:to_pair
  )
;;

let mkdirs ps =
  Or_error.(
    tag ~tag:"Couldn't make directories"
      (try_with_join
         ( fun () ->
             Or_error.all_unit
               (List.map ~f:(Fn.compose Fs.Unix.mkdir_p snd) (all_dirs ps))
         )
      )
  )
;;

let make_id_path init id =
  let segs = Id.to_string_list id in
  List.fold ~init ~f:(Fpath.add_seg) segs
;;

let%expect_test "make_id_path: folds in correct direction" =
  let id = Id.of_string "foo.bar.baz" in
  let init = Fpath.v "." in
  Io.print_bool
    (Fpath.equal
       (make_id_path init id)
       Fpath.(init / "foo" / "bar" / "baz")
    );
  [%expect {| true |}]
;;

let make_c_path (id_path : Fpath.t) : Input_mode.t -> Fpath.t =
  function
  | Memalloy in_root -> Fpath.(in_root / "C")
  | Litmus_only _    -> Fpath.(id_path / "c")
;;

module Get_files (F : Fs.S) = struct
  let get_memalloy_litc_files
      (input_root : Fpath.t) : Fpath.t list Or_error.t =
    let open Or_error.Let_syntax in
    let litmus_dir = Fpath.(input_root / "Litmus") in
    let%map files =
      F.get_files ~ext:"litmus" (Fpath.(input_root / "Litmus"))
    in
    List.map ~f:(Fpath.append litmus_dir) files
end

include Get_files (Fs.Unix)

let get_litc_files : Input_mode.t -> Fpath.t list Or_error.t =
  function
  | Memalloy input_root -> get_memalloy_litc_files input_root
  | Litmus_only files -> Or_error.return files
;;

let make
  (id : Id.t)
  ~(input_mode  : Input_mode.t)
  ~(output_root : Fpath.t)
  : t Or_error.t =
  let open Or_error.Let_syntax in
  let id_path = make_id_path output_root id in
  let%map litc_files = get_litc_files input_mode in
  Fpath.
    { input_mode
    ; litc_files
    ; c_path    = make_c_path id_path input_mode
    ; asm_path  = id_path / "asm"
    ; lita_path = id_path / "litmus"
    ; herd_path = id_path / "herd"
    }
;;

(* TODO(@MattWindsor91) Needs porting to new pathset.
let%test_module "all_paths" = (module struct
  let run_test ~input_mode =
    let id = Id.of_string "foo.bar.baz" in
    let config = Config.make ~out_root:(Fpath.v "outputs") ~input_mode in
    let ps = make config id in
    let all = all_paths ps in
    let all_str = List.Assoc.map ~f:Fpath.segs all in
    Format.printf "@[%a@]@."
      Sexp.pp_hum
      [%sexp (all_str : (string, string list) List.Assoc.t)]
  ;;

  let%expect_test "all_paths of make (memalloy mode)" =
    run_test ~input_mode:(`Memalloy (Fpath.v "inputs"));
    [%expect {|
    ((c_path (inputs C)) (litc_path (inputs Litmus))
     (asm_path (outputs foo bar baz asm))
     (lita_path (outputs foo bar baz litmus))
     (herd_path (outputs foo bar baz herd))) |}]
  ;;

  let%expect_test "all_paths of make (delitmus mode)" =
    run_test ~input_mode:`Delitmus;
    [%expect {|
    ((c_path (inputs)) (litc_path (inputs)) (asm_path (outputs foo bar baz asm))
     (lita_path (outputs foo bar baz litmus))
     (herd_path (outputs foo bar baz herd))) |}]
  ;;
end)
*)

let make_and_mkdirs
  (id : Id.t)
  ~(input_mode  : Input_mode.t)
  ~(output_root : Fpath.t)
  : t Or_error.t =
  Or_error.(
    make id ~input_mode ~output_root
    >>= Travesty.T_or_error.tee_m ~f:mkdirs
  )
;;

(* TODO(@MattWindsor91): needs fixing up for new pathsets. *)
let pp : t Fmt.t =
  let p f (k, v) =
    My_format.pp_kv f k String.pp (My_filename.concat_list v)
  in
  Fmt.(
    using (fun ps -> (List.Assoc.map ~f:Fpath.segs (all_dirs ps)))
    (vbox (list ~sep:cut p))
  )
;;
