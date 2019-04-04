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
open Lib
open Utils

type spec = {c_globals: string sexp_list; c_locals: string sexp_list}
[@@deriving sexp]

let find_spec specs (path : Fpath.t) =
  let file = Fpath.to_string path in
  file
  |> List.Assoc.find specs ~equal:String.Caseless.equal
  |> Result.of_option
       ~error:
         (Error.create_s
            [%message "File mentioned in spec is missing" ~file])

let read_specs path =
  let spec_file = Fpath.(path / "spec") in
  Or_error.try_with (fun () ->
      Sexp.load_sexp_conv_exn
        (Fpath.to_string spec_file)
        [%of_sexp: (string, spec) List.Assoc.t] )

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

let regress_run_asm (module L : Asm_job.Runner) (dir : Fpath.t) mode specs
    passes (file : Fpath.t) =
  let open Or_error.Let_syntax in
  let%bind filepath = to_full_path ~dir ~file in
  let%bind spec = find_spec specs file in
  let symbols = spec.c_globals @ spec.c_locals (* for now *) in
  let input = Asm_job.make ~passes ~symbols in
  let%map _ =
    match mode with
    | `Litmusify ->
        L.Litmusify.run_from_fpaths (input ()) ~infile:(Some filepath)
          ~outfile:None
    | `Explain ->
        L.Explain.run_from_fpaths (input ()) ~infile:(Some filepath)
          ~outfile:None
  in
  ()

let regress_on_files (bin_name : string) (test_dir : Fpath.t)
    ~(ext : string) ~(f : Fpath.t -> unit Or_error.t) : unit Or_error.t =
  let open Or_error.Let_syntax in
  printf "# %s tests\n\n" bin_name ;
  let%bind test_files = Fs.Unix.get_files ~ext test_dir in
  let results =
    List.map test_files ~f:(fun file ->
        Fmt.pr "## %a\n\n```@." Fpath.pp file ;
        Out_channel.flush stdout ;
        let%map () = f file in
        printf "```\n" )
  in
  let%map () = Or_error.combine_errors_unit results in
  printf "\nRan %d test(s).\n" (List.length test_files)

let regress_run_asm_many (modename : string) mode passes
    (test_path : Fpath.t) : unit Or_error.t =
  let open Or_error.Let_syntax in
  let arch = Config.Id.of_string "x86.att" in
  let path = Fpath.(test_path / "asm" / "x86" / "att" / "") in
  let%bind l = Language_support.asm_runner_from_arch arch in
  let%bind specs = read_specs path in
  let%bind test_files = Fs.Unix.get_files ~ext:"s" path in
  let%bind () = check_files_against_specs specs test_files in
  regress_on_files modename path ~ext:"s"
    ~f:(regress_run_asm l path mode specs passes)

let regress_explain : Fpath.t -> unit Or_error.t =
  regress_run_asm_many "Explainer" `Explain Config.Sanitiser_pass.explain

let regress_litmusify : Fpath.t -> unit Or_error.t =
  regress_run_asm_many "Litmusifier" `Litmusify
    Config.Sanitiser_pass.standard

let pp_cvars : Config.C_variables.Map.t Fmt.t =
  Fmt.(
    prefix
      (unit "@,@,// C variables:@,")
      (vbox
         (using C_identifier.Map.keys
            (list ~sep:sp (hbox (prefix (unit "// -@ ") C_identifier.pp))))))

let pp_post : C.Mini_litmus.Ast.Postcondition.t Fmt.t =
  Fmt.(
    prefix
      (unit "@,@,// Postcondition:@,")
      (hbox (prefix (unit "// ") C.Mini_litmus.Pp.pp_post)))

let summarise_c_output (o : C.Filters.Output.t) : unit =
  Fmt.(
    pr "@[<v>%a%a@]@." pp_cvars
      (C.Filters.Output.cvars o)
      (option pp_post) (C.Filters.Output.post o))

let delitmus_file (dir : Fpath.t) (file : Fpath.t) : unit Or_error.t =
  let open Or_error.Let_syntax in
  let%bind path = to_full_path ~dir ~file in
  let%map output =
    C.Filters.Litmus.run_from_fpaths C.Filters.Delitmus ~infile:(Some path)
      ~outfile:None
  in
  summarise_c_output output

let regress_delitmus (test_dir : Fpath.t) : unit Or_error.t =
  regress_on_files "Delitmus" test_dir ~ext:"litmus"
    ~f:(delitmus_file test_dir)

let parse_config_failures (dir : Fpath.t) (file : Fpath.t) : unit Or_error.t
    =
  let open Or_error.Let_syntax in
  let%bind path = to_full_path ~dir ~file in
  let r = Config.Frontend.load ~path in
  Fmt.(pr "@[%a@]@." (result ~ok:(always "No failures.") ~error:Error.pp)) r ;
  Result.ok_unit

let regress_config_parser_failures (test_dir : Fpath.t) : unit Or_error.t =
  regress_on_files "Config parser (failures)" test_dir ~ext:"conf"
    ~f:(parse_config_failures test_dir)

let make_regress_command ~(summary : string)
    (regress_function : Fpath.t -> unit Or_error.t) : Command.t =
  let open Command.Let_syntax in
  Command.basic ~summary
    [%map_open
      let test_path_raw = anon ("TEST_PATH" %: string) in
      fun () ->
        let o = Output.make ~verbose:false ~warnings:true in
        Or_error.(test_path_raw |> Io.fpath_of_string >>= regress_function)
        |> Output.print_error o]

let command : Command.t =
  Command.group ~summary:"runs regression tests"
    [ ( "explain"
      , make_regress_command ~summary:"runs explainer regressions"
          regress_explain )
    ; ( "litmusify"
      , make_regress_command ~summary:"runs litmusifier regressions"
          regress_litmusify )
    ; ( "delitmus"
      , make_regress_command ~summary:"runs de-litmus regressions"
          regress_delitmus )
    ; ( "config-failures"
      , make_regress_command
          ~summary:"runs config parser failure regressions"
          regress_config_parser_failures ) ]
