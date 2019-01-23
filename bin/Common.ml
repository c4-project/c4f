(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

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

let warn_if_not_tracking_symbols (o : Output.t)
  : string list option -> unit = function
  | None ->
    Format.fprintf o.wf
      "@[%a@]@."
      (Format.pp_print_list ~pp_sep:Format.pp_print_space String.pp)
      [ "The set of known C variables is empty."
      ; "This can lead to `act` making incorrect assumptions;"
      ; "for example, it may fail to work out which assembly symbols"
      ; "refer to heap locations."
      ; "To fix this, specify `-cvars 'symbol1,symbol2,etc'`."
      ]
  | Some _ -> ()
;;

let get_target cfg = function
  | `Id id ->
    let open Or_error.Let_syntax in
    let%map spec = Compiler.Spec.Set.get (Lib.Config.M.compilers cfg) id in
    `Spec spec
  | `Arch _ as arch -> Or_error.return arch
;;

let asm_runner_of_target
  (tgt : Compiler.Target.t) : (module Asm_job.Runner) Or_error.t =
  Language_support.asm_runner_from_arch (Compiler.Target.arch tgt)
;;

module Chain_with_delitmus
    (Onto  : Filter.S)
  : Filter.S with type aux_i = (File_type.t_or_infer * (C.Filters.Output.t Filter.chain_output -> Onto.aux_i))
              and type aux_o = (C.Filters.Output.t option * Onto.aux_o) =
  Filter.Chain_conditional_first (struct
    module First  = C.Filters.Litmus
    module Second = Onto
    type aux_i = (File_type.t_or_infer * (C.Filters.Output.t Filter.chain_output -> Onto.aux_i))

    let select { Filter.aux = (file_type, rest); src; _ } =
      if File_type.is_c_litmus src file_type
      then `Both (C.Filters.Delitmus, rest)
      else `One  rest
  end)
;;

let chain_with_delitmus
  (type aux_i)
  (type aux_o)
  (module Onto : Filter.S with type aux_i = aux_i and type aux_o = aux_o)
  : ( module Filter.S with type aux_i = (File_type.t_or_infer * (C.Filters.Output.t Filter.chain_output -> aux_i))
                       and type aux_o = (C.Filters.Output.t option * aux_o)
    ) =
  (module Chain_with_delitmus (Onto)
     : Filter.S with type aux_i = (File_type.t_or_infer * (C.Filters.Output.t Filter.chain_output -> aux_i))
                 and type aux_o = (C.Filters.Output.t option * aux_o)
  )
;;

let delitmus_compile_asm_pipeline
    (type i)
    (type o)
    (target : Compiler.Target.t)
    (job_maker : (module Asm_job.Runner) ->
     (module Filter.S with type aux_i = i and type aux_o = o))
  : (module
      Filter.S with type aux_i =
                      ( File_type.t_or_infer
                        * ( C.Filters.Output.t Filter.chain_output
                            -> i Compiler.Chain_input.t
                          )
                      )
                and type aux_o =
                      ( C.Filters.Output.t option
                        * ( unit option * o )
                      )
    ) Or_error.t =
  let open Or_error in
  target
  |>  asm_runner_of_target
  >>| job_maker
  >>= Language_support.Resolve_compiler_from_target.chained_filter_from_spec
    target
  >>| chain_with_delitmus
;;

let explain_pipeline
  (target : Compiler.Target.t)
  : ( module
      Filter.S with type aux_i =
                      ( File_type.t_or_infer
                        * ( C.Filters.Output.t Filter.chain_output
                            ->
                            Asm_job.Explain_config.t
                              Asm_job.t
                              Compiler.Chain_input.t
                          )
                      )
                and type aux_o =
                      ( C.Filters.Output.t option
                        * ( unit option * Asm_job.output )
                      )
    ) Or_error.t =
  delitmus_compile_asm_pipeline target Asm_job.get_explain
;;

let litmusify_pipeline
  (target : Compiler.Target.t)
  : ( module
      Filter.S with type aux_i =
                      ( File_type.t_or_infer
                        * ( C.Filters.Output.t Filter.chain_output
                            ->
                            Asm_job.Litmus_config.t
                              Asm_job.t
                              Compiler.Chain_input.t
                          )
                      )
                and type aux_o =
                      ( C.Filters.Output.t option
                        * ( unit option * Asm_job.output )
                      )
    ) Or_error.t =
  delitmus_compile_asm_pipeline target Asm_job.get_litmusify
;;

let choose_cvars_after_delitmus
    (o : Output.t)
    (user_cvars : string list option)
    (dl_cvars : string list option)
  : string list option =
  (* We could use Option.first_some here, but expanding it out gives
     us the ability to verbose-log what we're doing. *)
  let out_cvars message cvars =
    Fmt.(
      pf o.vf "Using %s:@ %a@."
        message
        (list ~sep:comma string) cvars
    );
    Some cvars
  in
  match user_cvars, dl_cvars with
  | Some cvars, Some _ ->
    out_cvars "user-supplied cvars (overriding those found during delitmus)"
      cvars
  | Some cvars, None ->
    out_cvars "user-supplied cvars (none found during delitmus)" cvars
  | None, Some cvars ->
    out_cvars "cvars found during delitmus" cvars
  | None, None -> None
;;

let choose_cvars
  (o : Output.t)
  (user_cvars : string list option)
  (dl_output : C.Filters.Output.t Filter.chain_output)
  : string list option =
  let warn_if_empty, cvars = match dl_output with
    | `Checking_ahead -> false, None
    | `Skipped -> true, user_cvars
    | `Ran dl ->
      let dl_cvars = C.Filters.Output.cvars dl in
      true, choose_cvars_after_delitmus o user_cvars dl_cvars
  in
  if warn_if_empty then
    warn_if_not_tracking_symbols o cvars;
  cvars
;;

let make_compiler_input
  (o : Output.t)
  (file_type : File_type.t_or_infer)
  (user_cvars : string list option)
  (config : 'cfg)
  (passes : Sanitiser_pass.Set.t)
  (dl_output : C.Filters.Output.t Filter.chain_output)
  : 'cfg Asm_job.t
      Compiler.Chain_input.t =
  let cvars = choose_cvars o user_cvars dl_output in
  let litmus_job =
    Asm_job.make ~passes ~config ?symbols:cvars ()
  in
  Compiler.Chain_input.create
    ~file_type:(File_type.delitmusified file_type)
    ~next:(Fn.const litmus_job)
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
    (Standard_args.config_file standard_args)
    |> Io.fpath_of_string
    >>= Language_support.load_and_process_config
      ?compiler_predicate
      ?machine_predicate
      ?sanitiser_passes
      ?with_compiler_tests
    >>= f o
  ) |> Output.print_error o
;;
