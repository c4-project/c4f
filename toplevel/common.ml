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

let warn_if_not_tracking_symbols (o : Output.t) :
    C_identifier.t list option -> unit = function
  | None ->
      Fmt.(
        pf o.wf "@[%a@]@." (list ~sep:sp string)
          [ "The set of known C variables is empty."
          ; "This can lead to `act` making incorrect assumptions;"
          ; "for example, it may fail to work out which assembly symbols"
          ; "refer to heap locations."
          ; "To fix this, specify `-cvars 'symbol1,symbol2,etc'`." ])
  | Some _ ->
      ()

let get_target cfg = function
  | `Id id ->
      let open Or_error.Let_syntax in
      let%map spec =
        Config.Compiler.Spec.Set.get (Config.Act.compilers cfg) id
      in
      `Spec spec
  | `Arch _ as arch ->
      Or_error.return arch

let asm_runner_of_target (tgt : Config.Compiler.Target.t) :
    (module Asm_job.Runner) Or_error.t =
  Language_support.asm_runner_from_arch (Config.Compiler.Target.arch tgt)

module Chain_with_delitmus (Onto : Filter.S) :
  Filter.S
  with type aux_i =
              Config.File_type.t_or_infer
              * (C.Filters.Output.t Filter.chain_output -> Onto.aux_i)
   and type aux_o = C.Filters.Output.t option * Onto.aux_o =
Filter.Chain_conditional_first (struct
  module First = C.Filters.Litmus
  module Second = Onto

  type aux_i =
    Config.File_type.t_or_infer
    * (C.Filters.Output.t Filter.chain_output -> Onto.aux_i)

  let select {Filter.aux= file_type, rest; src; _} =
    if Config.File_type.is_c_litmus src file_type then
      `Both (C.Filters.Delitmus, rest)
    else `One rest
end)

let chain_with_delitmus (type aux_i aux_o)
    (module Onto : Filter.S with type aux_i = aux_i and type aux_o = aux_o)
    :
    (module Filter.S
       with type aux_i = Config.File_type.t_or_infer
                         * (C.Filters.Output.t Filter.chain_output -> aux_i)
        and type aux_o = C.Filters.Output.t option * aux_o) =
  (module Chain_with_delitmus (Onto)
  : Filter.S
    with type aux_i = Config.File_type.t_or_infer
                      * (C.Filters.Output.t Filter.chain_output -> aux_i)
     and type aux_o = C.Filters.Output.t option * aux_o )

let delitmus_compile_asm_pipeline (type i o)
    (target : Config.Compiler.Target.t)
    (job_maker :
         (module Asm_job.Runner)
      -> (module Filter.S with type aux_i = i and type aux_o = o)) :
    (module Filter.S
       with type aux_i = Config.File_type.t_or_infer
                         * (   C.Filters.Output.t Filter.chain_output
                            -> i Config.Compiler.Chain_input.t)
        and type aux_o = C.Filters.Output.t option * (unit option * o))
    Or_error.t =
  let open Or_error in
  target |> asm_runner_of_target >>| job_maker
  >>= Language_support.Resolve_compiler_from_target.chained_filter_from_spec
        target
  >>| chain_with_delitmus

let explain_pipeline (target : Config.Compiler.Target.t) :
    (module Filter.S
       with type aux_i = Config.File_type.t_or_infer
                         * (   C.Filters.Output.t Filter.chain_output
                            -> Asm_job.Explain_config.t Asm_job.t
                               Config.Compiler.Chain_input.t)
        and type aux_o = C.Filters.Output.t option
                         * (unit option * Asm_job.Output.t))
    Or_error.t =
  delitmus_compile_asm_pipeline target Asm_job.get_explain

let litmusify_pipeline (target : Config.Compiler.Target.t) :
    (module Filter.S
       with type aux_i = Config.File_type.t_or_infer
                         * (   C.Filters.Output.t Filter.chain_output
                            -> Sexp.t Litmusifier.Config.t Asm_job.t
                               Config.Compiler.Chain_input.t)
        and type aux_o = C.Filters.Output.t option
                         * (unit option * Asm_job.Output.t))
    Or_error.t =
  delitmus_compile_asm_pipeline target Asm_job.get_litmusify_sexp

let choose_cvars_after_delitmus (o : Output.t)
    (user_cvars : Config.C_variables.Map.t option)
    (dl_cvars : Config.C_variables.Map.t) : Config.C_variables.Map.t =
  (* We could use Option.first_some here, but expanding it out gives us the
     ability to verbose-log what we're doing. *)
  let out_cvars message cvars =
    Fmt.(
      pf o.vf "Using %s:@ %a@." message
        (using C_identifier.Map.keys (list ~sep:comma C_identifier.pp))
        cvars) ;
    cvars
  in
  match user_cvars with
  | Some cvars ->
      out_cvars
        "user-supplied cvars (overriding those found during delitmus)" cvars
  | None ->
      out_cvars "cvars found during delitmus" dl_cvars

let choose_cvars_inner (o : Output.t)
    (user_cvars : Config.C_variables.Map.t option) :
       C.Filters.Output.t Filter.chain_output
    -> bool * Config.C_variables.Map.t option = function
  | `Checking_ahead ->
      (false, None)
  | `Skipped ->
      (true, user_cvars)
  | `Ran dl ->
      let dl_cvars = C.Filters.Output.cvars dl in
      (true, Some (choose_cvars_after_delitmus o user_cvars dl_cvars))

let choose_cvars (o : Output.t)
    (user_cvars : Config.C_variables.Map.t option)
    (dl_output : C.Filters.Output.t Filter.chain_output) :
    Config.C_variables.Map.t option =
  let warn_if_empty, cvars = choose_cvars_inner o user_cvars dl_output in
  if warn_if_empty then
    warn_if_not_tracking_symbols o
      (Option.map ~f:C_identifier.Map.keys cvars) ;
  cvars

module T_opt = Travesty.T_option.With_errors

let collect_cvars ?(c_globals : string list option)
    ?(c_locals : string list option) () :
    Config.C_variables.Map.t option Or_error.t =
  let open Or_error.Let_syntax in
  let module V = Config.C_variables in
  let%bind globals =
    T_opt.map_m
      ~f:(V.String_lang.parse_list ~scope:V.Scope.Global)
      c_globals
  in
  let%map locals =
    T_opt.map_m ~f:(V.String_lang.parse_list ~scope:V.Scope.Local) c_locals
  in
  Option.merge globals locals ~f:(fun x y -> V.Map.merge_list [x; y])

let make_compiler_input (o : Output.t)
    (file_type : Config.File_type.t_or_infer)
    (user_cvars : Config.C_variables.Map.t option)
    (config_fn : c_variables:Config.C_variables.Map.t option -> 'cfg)
    (passes : Config.Sanitiser_pass.Set.t)
    (dl_output : C.Filters.Output.t Filter.chain_output) :
    'cfg Asm_job.t Config.Compiler.Chain_input.t =
  let c_variables = choose_cvars o user_cvars dl_output in
  let symbols =
    c_variables
    |> Option.map ~f:C_identifier.Map.keys
    |> Option.map ~f:(List.map ~f:C_identifier.to_string)
  in
  let config = config_fn ~c_variables in
  let litmus_job = Asm_job.make ~passes ~config ?symbols () in
  Config.Compiler.Chain_input.make
    ~file_type:(Config.File_type.delitmusified file_type)
    ~next:(Fn.const litmus_job)

let make_output_from_standard_args (args : Args.Standard.t) : Output.t =
  Output.make
    ~verbose:(Args.Standard.is_verbose args)
    ~warnings:(Args.Standard.are_warnings_enabled args)

(** Used as input to {{!Make_lifter} Make_lifter}. *)
module type Basic_lifter = sig
  type t

  val as_standard_args : t -> Args.Standard.t
end

let setup_colour (args : Args.Standard.t) : unit =
  let style_renderer = Args.Standard.colour args in
  Fmt_tty.setup_std_outputs ?style_renderer ()

(** Since the command lifters are fairly uniform except for the specific
    argument bundle they pass through, we use a functor to construct them. *)
module Make_lifter (B : Basic_lifter) = struct
  let lift ?compiler_predicate ?machine_predicate ?sanitiser_passes
      ?with_compiler_tests
      ~(f : B.t -> Output.t -> Config.Act.t -> unit Or_error.t) (args : B.t)
      : unit =
    let standard_args = B.as_standard_args args in
    setup_colour standard_args ;
    let o = make_output_from_standard_args standard_args in
    Or_error.(
      Args.Standard.config_file standard_args
      |> Io.fpath_of_string
      >>= Language_support.load_and_process_config ?compiler_predicate
            ?machine_predicate ?sanitiser_passes ?with_compiler_tests
      >>= f args o)
    |> Output.print_error o
end

module Standard_lifter = Make_lifter (struct
  type t = Args.Standard.t

  let as_standard_args = Fn.id
end)

let lift_command = Standard_lifter.lift

module With_files_lifter = Make_lifter (Args.Standard_with_files)

let lift_command_with_files = With_files_lifter.lift
