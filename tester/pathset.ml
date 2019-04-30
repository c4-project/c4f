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
open Travesty_base_exts
open Utils

let validate_fpath (f : Fpath.t -> bool) (expect : string) (fp : Fpath.t) :
    Validate.t =
  if f fp then Validate.pass
  else
    Validate.fail_s
      [%message
        (Printf.sprintf "Expected a %s here" expect)
          ~got:(Fpath.to_string fp : string)]

let validate_dir : Fpath.t Validate.check =
  validate_fpath Fpath.is_dir_path "directory path"

let validate_file : Fpath.t Validate.check =
  validate_fpath Fpath.is_file_path "file path"

let to_sim_file (dir : Fpath.t) (name : string) : Fpath.t =
  Fpath.((dir / name) + "sim.txt")

let make_id_dir init id =
  let segs = Config.Id.to_string_list id in
  Fpath.to_dir_path (List.fold ~init ~f:Fpath.add_seg segs)

let make_c_dir (id_dir : Fpath.t) : Input_mode.t -> Fpath.t =
  Input_mode.reduce
    ~memalloy:(fun in_root -> Fpath.(in_root / "C" / ""))
    ~litmus_only:(fun _ -> Fpath.(id_dir / "c" / ""))

let%expect_test "make_id_path: folds in correct direction" =
  let id = Config.Id.of_string "foo.bar.baz" in
  let init = Fpath.v "." in
  Io.print_bool
    (Fpath.equal (make_id_dir init id)
       Fpath.(init / "foo" / "bar" / "baz" / "")) ;
  [%expect {| true |}]

module Get_files (F : Fs.S) = struct
  let get_memalloy_litc_files (input_root : Fpath.t) :
      Fpath.t list Or_error.t =
    let open Or_error.Let_syntax in
    let litmus_dir = Fpath.(input_root / "Litmus" / "") in
    let%map files =
      F.get_files ~ext:"litmus" Fpath.(input_root / "Litmus" / "")
    in
    List.map ~f:(Fpath.append litmus_dir) files
end

module type Basic = sig
  type t

  type input

  val validate : t Validate.check

  val make_raw : input -> t Or_error.t

  val all_dirs : t -> (string, Fpath.t) List.Assoc.t
end

module Extend (B : Basic) = struct
  let mkdirs (ps : B.t) : unit Or_error.t =
    Or_error.(
      tag ~tag:"Couldn't make directories"
        (try_with_join (fun () ->
             Or_error.all_unit
               (List.map ~f:(Fn.compose Fs.Unix.mkdir_p snd) (B.all_dirs ps))
         )))

  let validate_err (t : B.t) : B.t Or_error.t =
    Validate.valid_or_error t B.validate

  let make (i : B.input) : B.t Or_error.t =
    Or_error.(i |> B.make_raw >>= validate_err)

  let make_and_mkdirs (i : B.input) : B.t Or_error.t =
    Or_error.(i |> make >>= Or_error.tee_m ~f:mkdirs)

  let pp : B.t Fmt.t =
    let p f (k, v) =
      My_format.pp_kv f k String.pp (My_filename.concat_list v)
    in
    Fmt.(
      using
        (fun ps -> List.Assoc.map ~f:Fpath.segs (B.all_dirs ps))
        (vbox (list ~sep:cut p)))
end

module Run = struct
  module M = struct
    type t =
      { c_litmus_files: Fpath.t list
      ; input_mode: Input_mode.t
      ; output_root_dir: Fpath.t
      ; c_sim_dir: Fpath.t }
    [@@deriving fields]

    type input = {input_mode: Input_mode.t; output_root_dir: Fpath.t}

    let all_dirs (ps : t) : (string, Fpath.t) List.Assoc.t =
      let to_pair fld = Some (Field.name fld, Field.get fld ps) in
      List.filter_opt
        (Fields.to_list ~input_mode:(Fn.const None)
           ~c_litmus_files:(Fn.const None) ~output_root_dir:to_pair
           ~c_sim_dir:to_pair)

    module GF = Get_files (Fs.Unix)

    let get_litc_files : Input_mode.t -> Fpath.t list Or_error.t =
      Input_mode.reduce ~memalloy:GF.get_memalloy_litc_files
        ~litmus_only:Or_error.return

    let validate_litc_files : Fpath.t list Validate.check =
      Validate.list_indexed validate_file

    let validate (ps : t) : Validate.t =
      let module V = Validate in
      let w check = V.field_folder ps check in
      V.of_list
        (Fields.fold ~init:[] ~c_litmus_files:(w validate_litc_files)
           ~input_mode:(w (Fn.const V.pass))
           ~output_root_dir:(w validate_dir) ~c_sim_dir:(w validate_dir))

    let make_raw ({input_mode; output_root_dir} : input) : t Or_error.t =
      Or_error.Let_syntax.(
        let%map c_litmus_files = get_litc_files input_mode in
        { c_litmus_files
        ; input_mode
        ; output_root_dir
        ; c_sim_dir= Fpath.(output_root_dir / "c_sim" / "") })
  end

  include M
  include Extend (M)

  let c_sim_file (ps : t) : string -> Fpath.t = to_sim_file (c_sim_dir ps)
end

module Compiler = struct
  module M = struct
    type t =
      { run: Run.t
      ; c_dir: Fpath.t
      ; asm_dir: Fpath.t
      ; lita_dir: Fpath.t
      ; asm_sim_dir: Fpath.t }
    [@@deriving fields]

    type input = {run: Run.t; compiler_id: Config.Id.t}

    let all_dirs (ps : t) : (string, Fpath.t) List.Assoc.t =
      let to_pair fld = [(Field.name fld, Field.get fld ps)] in
      List.concat
        (Fields.to_list
           ~run:(fun fld -> Run.all_dirs (Field.get fld ps))
           ~c_dir:to_pair ~asm_dir:to_pair ~lita_dir:to_pair
           ~asm_sim_dir:to_pair)

    let validate (ps : t) : Validate.t =
      let module V = Validate in
      let w check = V.field_folder ps check in
      V.of_list
        (Fields.fold ~init:[] ~run:(w Run.validate) ~c_dir:(w validate_dir)
           ~asm_dir:(w validate_dir) ~lita_dir:(w validate_dir)
           ~asm_sim_dir:(w validate_dir))

    let make_raw ({run; compiler_id} : input) : t Or_error.t =
      Or_error.return
        (let id_dir = make_id_dir (Run.output_root_dir run) compiler_id in
         Fpath.
           { run
           ; c_dir= make_c_dir id_dir (Run.input_mode run)
           ; asm_dir= id_dir / "asm" / ""
           ; lita_dir= id_dir / "litmus" / ""
           ; asm_sim_dir= Fpath.(id_dir / "asm_sim" / "") })
  end

  include M
  include Extend (M)

  let c_litmus_files : t -> Fpath.t list = Fn.compose Run.c_litmus_files run

  let input_mode : t -> Input_mode.t = Fn.compose Run.input_mode run

  let output_root_dir : t -> Fpath.t = Fn.compose Run.output_root_dir run

  let c_sim_file (ps : t) (name : string) : Fpath.t =
    Run.c_sim_file (run ps) name

  let asm_sim_file (ps : t) : string -> Fpath.t =
    to_sim_file (asm_sim_dir ps)
end

module File = struct
  type ps = Compiler.t

  type t =
    { name: string
    ; c_file: Fpath.t
    ; c_litmus_file: Fpath.t
    ; c_sim_file: Fpath.t
    ; asm_file: Fpath.t
    ; asm_litmus_file: Fpath.t
    ; asm_sim_file: Fpath.t }
  [@@deriving fields]

  let make (ps : ps) (c_litmus_file : Fpath.t) =
    let name = Fpath.(basename (rem_ext c_litmus_file)) in
    Fpath.
      { name
      ; c_litmus_file
      ; c_file= (Compiler.c_dir ps / name) + "c"
      ; asm_file= (Compiler.asm_dir ps / name) + "s"
      ; asm_litmus_file= (Compiler.lita_dir ps / name) + ".s.litmus"
      ; c_sim_file= Compiler.c_sim_file ps name
      ; asm_sim_file= Compiler.asm_sim_file ps name }

  let make_all (ps : Compiler.t) : t list =
    List.map ~f:(make ps) (Compiler.c_litmus_files ps)
end
