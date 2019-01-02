(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

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
open Lib
open Utils

type spec =
  { cvars : string list
  }
[@@deriving sexp]
;;

let find_spec specs (path : Fpath.t) =
  let file = Fpath.to_string path in
  file
  |> List.Assoc.find specs ~equal:(String.Caseless.equal)
  |> Result.of_option
    ~error:(
      Error.create_s
        [%message "File mentioned in spec is missing" ~file]
    )
;;

let regress_run_asm ((module L) : (module Asm_job.Runner))
    (path : Fpath.t) mode specs passes (file : Fpath.t) =
  let open Or_error.Let_syntax in

  let%bind spec = find_spec specs file in

  Fmt.pr "## %a\n\n```\n@." Fpath.pp file;

  let input =
    { Asm_job.inp = Io.In_source.file Fpath.(path // file)
    ; outp = Io.Out_sink.stdout
    ; passes
    ; symbols = spec.cvars
    }
  in

  let%map _ = match mode with
    | `Litmusify -> L.litmusify input
    | `Explain   -> L.explain input
  in

  Out_channel.flush stdout;
  printf "```\n";
;;

let read_specs path =
  let spec_file = Fpath.(path / "spec") in
  Or_error.try_with
    (fun () ->
       Sexp.load_sexp_conv_exn (Fpath.to_string spec_file)
         [%of_sexp: (string, spec) List.Assoc.t]
    )
;;

let diff_to_error = function
  | First file ->
    Or_error.error_s
      [%message "File mentioned in test spec, but doesn't exist"
          ~file
      ]
  | Second file ->
    Or_error.error_s
      [%message "File in test directory, but doesn't have a spec"
          ~file
      ]
;;

let check_files_against_specs specs (test_paths : Fpath.t list) =
  let spec_set = specs |> List.map ~f:fst |> String.Set.of_list in
  let files_set =
    test_paths
    |> List.map ~f:Fpath.to_string
    |> String.Set.of_list
  in
  String.Set.symmetric_diff spec_set files_set
  |> Sequence.map ~f:diff_to_error
  |> Sequence.to_list
  |> Or_error.combine_errors_unit
;;

let regress_run_asm_many modename mode passes test_path =
  let open Or_error.Let_syntax in

  printf "# %s tests\n\n" modename;

  let emits = ["x86"; "att"] in
  let path = Fpath.(v test_path / "asm" / "x86" / "att") in
  let%bind l = Language_support.asm_runner_from_emits emits in
  let%bind specs = read_specs path in
  let%bind test_files = Io.Dir.get_files ~ext:"s" path in
  let%bind () = check_files_against_specs specs test_files in
  let results = List.map test_files
      ~f:(regress_run_asm l path mode specs passes)
  in
  let%map () = Or_error.combine_errors_unit results in
  printf "\nRan %d test(s).\n\n" (List.length test_files)
;;

let regress_explain =
  regress_run_asm_many "Explainer" `Explain
    (Sanitiser_pass.explain)
;;

let regress_litmusify =
  regress_run_asm_many "Litmusifier" `Litmusify
    (Sanitiser_pass.standard)
;;

let regress test_path =
  let open Or_error.Let_syntax in
  let%bind () = regress_explain test_path in
  let%map  () = regress_litmusify test_path in
  Out_channel.newline stdout
;;

let command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"runs regression tests"
    [%map_open
      let test_path = anon ("TEST_PATH" %: string) in
       fun () ->
         let o = Output.make ~verbose:false ~warnings:true in
         regress test_path |> Output.print_error o
    ]
;;
