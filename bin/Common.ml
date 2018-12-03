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

open Core_kernel
open Lib
open Utils

let warn_if_not_tracking_symbols (o : Output.t) = function
  | [] ->
    Format.fprintf o.wf
      "@[%a@]@."
      (Format.pp_print_list ~pp_sep:Format.pp_print_space String.pp)
      [ "The set of known C variables is empty."
      ; "This can lead to `act` making incorrect assumptions;"
      ; "for example, it may fail to work out which assembly symbols"
      ; "refer to heap locations."
      ; "To fix this, specify `-cvars 'symbol1,symbol2,etc'`."
      ]
  | _ :: _ -> ()
;;

let temp_file = Filename.temp_file "act"

let asm_file is_c maybe_infile =
  if is_c then Some (temp_file "s") else maybe_infile
;;

let decide_if_c infile = function
  | `C -> true
  | `Assembly -> false
  | `Infer ->
    Option.exists infile ~f:(My_filename.has_extension ~ext:"c")
;;

let get_target cfg = function
  | `Id id ->
    let open Or_error.Let_syntax in
    let%map spec = Compiler.Spec.Set.get (Config.M.compilers cfg) id in
    `Spec spec
  | `Arch _ as arch -> Or_error.return arch
;;

let arch_of_target = function
  | `Spec spec -> Compiler.Spec.With_id.emits spec
  | `Arch arch -> arch
;;

let runner_of_target = function
  | `Spec spec -> Language_support.asm_runner_from_spec spec
  | `Arch arch -> Language_support.asm_runner_from_emits arch
;;

let run_compiler target ~infile ~outfile =
  let open Result.Let_syntax in
  let%bind cspec = match target with
    | `Spec spec -> return spec
    | `Arch _ ->
      Or_error.error_string
        "To litmusify a C file, you must supply a compiler ID."
  in
  let%bind infile =
    Result.of_option infile
      ~error:(Error.of_string "Can't read in C from stdin")
  in
  let%bind outfile =
    Result.of_option outfile
      ~error:(Error.of_string "Can't output compiler result to stdout")
  in
  let%bind (module C) = Language_support.compiler_from_spec cspec in
  Or_error.tag ~tag:"While compiling to assembly"
    (C.compile ~infile ~outfile)
;;

let maybe_run_compiler target file_type infile =
  let open Result.Let_syntax in
  let is_c = decide_if_c infile file_type in
  let outfile = asm_file is_c infile in
  let%map () =
    if is_c then run_compiler target ~infile ~outfile else return ()
  in outfile
;;

let lift_command
    ?compiler_predicate
    ?machine_predicate
    ?with_compiler_tests
    ~f
    standard_args
  =
  let o =
    Output.make
      ~verbose:(Standard_args.is_verbose standard_args)
      ~warnings:(Standard_args.are_warnings_enabled standard_args)
  in
  Or_error.(
    Language_support.load_and_process_config
      ?compiler_predicate
      ?machine_predicate
      ?with_compiler_tests
      (Standard_args.spec_file standard_args)
    >>= f o
  ) |> Output.print_error o
;;

let litmusify ?output_format (o : Output.t) inp outp symbols
    target =
  let open Result.Let_syntax in
  let%bind (module Runner) = runner_of_target target in
  let input =
    { Asm_job.inp
    ; outp
    ; passes = Sanitiser_pass.all_set ()
    ; symbols
    }
  in
  let%map output =
    Or_error.tag ~tag:"While translating assembly to litmus"
      (Runner.litmusify ?output_format input)
  in
  Asm_job.warn output o.wf;
  Asm_job.symbol_map output
;;
