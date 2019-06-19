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

open Core_kernel
module Ac = Act_common
module Au = Act_utils

module Spec = struct
  module M = struct
    type t = (string, Act_delitmus.Aux.t) List.Assoc.t

    module SRM : Monad.S with type 'a t = ('a, string) Result.t = struct
      type 'a t = ('a, string) Result.t

      include (Result : Monad.S with type 'a t := ('a, string) Result.t)
    end

    module List_of_alist =
      Travesty.Bi_traversable.Chain_Bi2_Traverse1
        (Travesty_base_exts.Alist)
        (Travesty_base_exts.List)
    module List_of_alist_SRM = List_of_alist.On_monad (SRM)

    let of_yojson (j : Yojson.Safe.t) : (t, string) Result.t =
      Result.Let_syntax.(
        let jsons =
          [j] |> Yojson.Safe.Util.filter_assoc
          |> List_of_alist_SRM.map_right_m ~f:Act_delitmus.Aux.of_yojson
        in
        match%bind jsons with
        | [x] ->
            Result.return x
        | _ ->
            Result.fail "Expected a JSON object here")

    let of_yojson_exn (j : Yojson.Safe.t) : t =
      j |> of_yojson |> Result.ok_or_failwith
  end

  include M

  module Load : Plumbing.Loadable_types.S with type t := t =
    Plumbing.Loadable.Of_jsonable (M)

  include Load
end

let find_aux (spec : Spec.t) (path : Fpath.t) :
    Act_delitmus.Aux.t Or_error.t =
  let file = Fpath.to_string path in
  file
  |> List.Assoc.find spec ~equal:String.Caseless.equal
  |> Result.of_option
       ~error:
         (Error.create_s
            [%message "File mentioned in spec is missing" ~file])

let read_specs (path : Fpath.t) : Spec.t Or_error.t =
  let spec_file = Fpath.(path / "spec.json") in
  Spec.load_from_isrc (Plumbing.Input.of_fpath spec_file)

let diff_to_error = function
  | First file ->
      Or_error.error_s
        [%message "File mentioned in test spec, but doesn't exist" ~file]
  | Second file ->
      Or_error.error_s
        [%message "File in test directory, but doesn't have a spec" ~file]

let validate_path_and_file : (Fpath.t * Fpath.t) Validate.check =
  Validate.(
    pair
      ~fst:
        (booltest Fpath.is_dir_path ~if_false:"path should be a directory")
      ~snd:(booltest Fpath.is_file_path ~if_false:"file should be a file"))

let to_full_path ~(dir : Fpath.t) ~(file : Fpath.t) : Fpath.t Or_error.t =
  Or_error.(
    Validate.valid_or_error (dir, file) validate_path_and_file
    >>| Tuple2.uncurry Fpath.append)

let check_files_against_specs specs (test_paths : Fpath.t list) =
  let spec_set = specs |> List.map ~f:fst |> String.Set.of_list in
  let files_set =
    test_paths |> List.map ~f:Fpath.to_string |> String.Set.of_list
  in
  String.Set.symmetric_diff spec_set files_set
  |> Sequence.map ~f:diff_to_error
  |> Sequence.to_list |> Or_error.combine_errors_unit

let regress_run_asm (type cfg)
    (module Job : Act_asm.Runner_intf.S with type cfg = cfg) (spec : Spec.t)
    (passes : Set.M(Act_sanitiser.Pass_group).t) ~(file : Fpath.t)
    ~(config_fn : Act_delitmus.Aux.t -> cfg) ~(path : Fpath.t) :
    unit Or_error.t =
  let open Or_error.Let_syntax in
  let%bind aux = find_aux spec file in
  let input = Act_asm.Job.make ~passes ~config:(config_fn aux) in
  let%map _ =
    Job.run (input ()) (Plumbing.Input.of_fpath path) Plumbing.Output.stdout
  in
  ()

let regress_on_file ~(dir : Fpath.t) (file : Fpath.t)
    ~(f : file:Fpath.t -> path:Fpath.t -> unit Or_error.t) : unit Or_error.t
    =
  Fmt.pr "## %a\n\n```@." Fpath.pp file ;
  Out_channel.flush stdout ;
  Or_error.Let_syntax.(
    let%bind path = to_full_path ~dir ~file in
    let%map () = f ~path ~file in
    printf "```\n")

let regress_on_files (bin_name : string) ~(dir : Fpath.t) ~(ext : string)
    ~(f : file:Fpath.t -> path:Fpath.t -> unit Or_error.t) : unit Or_error.t
    =
  let open Or_error.Let_syntax in
  printf "# %s tests\n\n" bin_name ;
  let%bind test_files = Au.Fs.Unix.get_files ~ext dir in
  let results = List.map test_files ~f:(regress_on_file ~dir ~f) in
  let%map () = Or_error.combine_errors_unit results in
  printf "\nRan %d test(s).\n" (List.length test_files)

let regress_run_asm_many (type cfg)
    (module Job : Act_asm.Runner_intf.S with type cfg = cfg)
    (modename : string) passes (test_path : Fpath.t)
    ~(config_fn : Act_delitmus.Aux.t -> cfg) : unit Or_error.t =
  let open Or_error.Let_syntax in
  let dir = Fpath.(test_path / "asm" / "x86" / "att" / "") in
  let%bind specs = read_specs dir in
  let%bind test_files = Au.Fs.Unix.get_files ~ext:"s" dir in
  let%bind () = check_files_against_specs specs test_files in
  regress_on_files modename ~dir ~ext:"s"
    ~f:(regress_run_asm (module Job) specs passes ~config_fn)

let make_regress_command (regress_function : Fpath.t -> unit Or_error.t)
    ~(summary : string) : Command.t =
  Command.basic ~summary
    Command.Let_syntax.(
      let%map_open test_path_raw = anon ("TEST_PATH" %: string) in
      fun () ->
        let o = Ac.Output.make ~verbose:false ~warnings:true in
        Or_error.(
          test_path_raw |> Plumbing.Fpath_helpers.of_string
          >>= regress_function)
        |> Ac.Output.print_error o)
