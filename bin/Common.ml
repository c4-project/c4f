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

let decide_if_c (infile : Fpath.t option)
  : [> `C | `Infer] -> bool = function
  | `C -> true
  | `Infer -> Option.exists infile ~f:(Fpath.has_ext "c")
  | _ -> false
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

let ensure_spec : [> `Spec of Compiler.Spec.With_id.t]
  -> Compiler.Spec.With_id.t Or_error.t = function
    | `Spec spec -> Or_error.return spec
    | _ ->
      Or_error.error_string
        "To handle C files, you must supply a compiler ID."
;;

let maybe_run_compiler
  (type aux)
  (module Onto : Filter.S with type aux = aux)
  (target : [< `Spec of Compiler.Spec.With_id.t | `Arch of string list > `Spec ])
  (file_type : [> `Assembly | `C | `Infer])
  : (module Filter.S with type aux = (unit option * aux)) Or_error.t =
  let open Result.Let_syntax in
  let%bind cspec = ensure_spec target in
  let%map (module C) = Language_support.compiler_filter_from_spec cspec in
  let should_run_compiler isrc
    = decide_if_c (Io.In_source.to_file isrc) file_type
  in
  (module
    Filter.Chain_conditional_first (struct
      module First = C
      module Second = Onto
      let condition src _snk = should_run_compiler src
    end) : Filter.S with type aux = (unit option * aux))
;;

let lift_command
    ?compiler_predicate
    ?machine_predicate
    ?sanitiser_passes
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
    (Standard_args.spec_file standard_args)
    |> Io.fpath_of_string
    >>= Language_support.load_and_process_config
      ?compiler_predicate
      ?machine_predicate
      ?sanitiser_passes
      ?with_compiler_tests
    >>= f o
  ) |> Output.print_error o
;;

let litmusify ?output_format (o : Output.t)
    passes symbols target inp outp
    : (string, string) List.Assoc.t Or_error.t =
  let open Result.Let_syntax in
  let%bind (module Runner) = runner_of_target target in
  let input =
    { Asm_job.inp
    ; outp
    ; passes
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

let litmusify_filter ?output_format (o : Output.t) passes symbols target
  : (module Filter.S with type aux = (string, string) List.Assoc.t) =
  (module
    (struct
      type aux = (string, string) List.Assoc.t
      let run = litmusify ?output_format o passes symbols target
      let run_from_string_paths = Filter.lift_to_raw_strings ~f:run
      let run_from_fpaths       = Filter.lift_to_fpaths ~f:run
    end) : Filter.S with type aux = (string, string) List.Assoc.t)
;;
