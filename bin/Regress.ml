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

let regress_run_asm ((module L) : (module Asm_job.Runner))
    path mode passes file =
  let open Or_error.Let_syntax in

  printf "## %s\n\n```\n" file;
  Out_channel.flush stdout;

  let input =
    { Asm_job.inp = `File (path ^/ file)
    ; outp = `Stdout
    ; passes
    ; symbols = [] (* TODO(@MattWindsor91): exercise this? *)
    }
  in

  let%map _ = match mode with
    | `Litmusify -> L.litmusify input
    | `Explain   -> L.explain input
  in

  Out_channel.flush stdout;
  printf "```\n";
;;

let regress_run_asm_many modename mode passes test_path =
  let open Or_error.Let_syntax in

  printf "# %s tests\n\n" modename;

  let emits = ["x86"; "att"] in
  let path = My_filename.concat_list ([test_path; "asm"] @ emits) in
  let%bind l = Language_support.asm_runner_from_emits emits in
  let%bind testfiles = Io.Dir.get_files ~ext:"s" path in
  let results = List.map testfiles
      ~f:(regress_run_asm l path mode passes)
  in
  let%map () = Or_error.combine_errors_unit results in
  printf "\nRan %d test(s).\n\n" (List.length testfiles)
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
