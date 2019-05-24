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
module Tx = Travesty_core_kernel_exts
module Ac = Act_common
open Act_utils

let not_tracking_symbols_warning : string =
  {|
    The set of known C variables is empty.

    This can lead to `act` making incorrect assumptions;
    for example, it may fail to work out which assembly symbols
    refer to heap locations.

    To fix this, specify `-c-globals 'symbol1,symbol2,etc'` (and/or `-c-locals`),
    or (if possible) use a C litmus test as input.
  |}

let warn_if_not_tracking_symbols (o : Ac.Output.t) :
    Ac.C_id.t list option -> unit = function
  | None ->
      Ac.Output.pw o "@[%a@]@." Fmt.paragraphs not_tracking_symbols_warning
  | Some _ ->
      ()

let asm_runner_of_target (tgt : Act_config.Compiler.Target.t) :
    (module Act_asm.Runner_intf.Basic) Or_error.t =
  Language_support.asm_runner_from_arch
    (Act_config.Compiler.Target.arch tgt)

module Chain_with_delitmus (Onto : Filter_intf.S) :
  Filter_intf.S
  with type aux_i =
              Ac.File_type.t
              * (   Act_c.Filters.Output.t Filter_intf.chain_output
                 -> Onto.aux_i)
   and type aux_o = Act_c.Filters.Output.t option * Onto.aux_o =
Filter.Chain_conditional_first (struct
  module First = Act_c.Filters.Litmus
  module Second = Onto

  type aux_i =
    Ac.File_type.t
    * (Act_c.Filters.Output.t Filter_intf.chain_output -> Onto.aux_i)

  let select {Filter_intf.aux= file_type, rest; src; _} =
    if Act_common.File_type.is_c_litmus src file_type then
      `Both (Act_c.Filters.Delitmus, rest)
    else `One rest
end)

let chain_with_delitmus (type aux_i aux_o)
    (module Onto : Filter_intf.S
      with type aux_i = aux_i
       and type aux_o = aux_o) =
  (module Chain_with_delitmus (Onto)
  : Filter_intf.S
    with type aux_i = Ac.File_type.t
                      * (   Act_c.Filters.Output.t Filter_intf.chain_output
                         -> aux_i)
     and type aux_o = Act_c.Filters.Output.t option * aux_o )

let delitmus_compile_asm_pipeline (type c)
    (target : Act_config.Compiler.Target.t)
    (job_maker :
         (module Act_asm.Runner_intf.Basic)
      -> (module Act_asm.Runner_intf.S with type cfg = c)) :
    (module Filter_intf.S
       with type aux_i = Ac.File_type.t
                         * (   Act_c.Filters.Output.t
                               Filter_intf.chain_output
                            -> c Act_asm.Job.t
                               Act_config.Compiler.Chain_input.t)
        and type aux_o = Act_c.Filters.Output.t option
                         * (unit option * Act_asm.Job.Output.t))
    Or_error.t =
  Or_error.Let_syntax.(
    let%bind (module J) = target |> asm_runner_of_target >>| job_maker in
    let%map with_compiler =
      Language_support.Resolve_compiler_from_target.chained_filter_from_spec
        target
        (module J)
    in
    chain_with_delitmus with_compiler)

let litmusify_pipeline (target : Act_config.Compiler.Target.t) :
    (module Filter_intf.S
       with type aux_i = Ac.File_type.t
                         * (   Act_c.Filters.Output.t
                               Filter_intf.chain_output
                            -> Sexp.t Act_asm.Litmusifier.Config.t
                               Act_asm.Job.t
                               Act_config.Compiler.Chain_input.t)
        and type aux_o = Act_c.Filters.Output.t option
                         * (unit option * Act_asm.Job.Output.t))
    Or_error.t =
  delitmus_compile_asm_pipeline target Act_asm.Litmusifier.get_filter

let choose_cvars_after_delitmus (o : Ac.Output.t)
    (user_cvars : Ac.C_variables.Map.t option)
    (dl_cvars : Ac.C_variables.Map.t) : Ac.C_variables.Map.t =
  (* We could use Option.first_some here, but expanding it out gives us the
     ability to verbose-log what we're doing. *)
  let out_cvars message cvars =
    Ac.Output.pv o "Using %s:@ %a@." message
      Fmt.(using Ac.C_id.Map.keys (list ~sep:comma Ac.C_id.pp))
      cvars ;
    cvars
  in
  match user_cvars with
  | Some cvars ->
      out_cvars
        "user-supplied cvars (overriding those found during delitmus)" cvars
  | None ->
      out_cvars "cvars found during delitmus" dl_cvars

let choose_cvars_inner (o : Ac.Output.t)
    (user_cvars : Ac.C_variables.Map.t option) :
       Act_c.Filters.Output.t Filter_intf.chain_output
    -> bool * Ac.C_variables.Map.t option = function
  | `Checking_ahead ->
      (false, None)
  | `Skipped ->
      (true, user_cvars)
  | `Ran dl ->
      let dl_cvars = Act_c.Filters.Output.cvars dl in
      (true, Some (choose_cvars_after_delitmus o user_cvars dl_cvars))

let choose_cvars (o : Ac.Output.t)
    (user_cvars : Ac.C_variables.Map.t option)
    (dl_output : Act_c.Filters.Output.t Filter_intf.chain_output) :
    Ac.C_variables.Map.t option =
  let warn_if_empty, cvars = choose_cvars_inner o user_cvars dl_output in
  if warn_if_empty then
    warn_if_not_tracking_symbols o (Option.map ~f:Ac.C_id.Map.keys cvars) ;
  cvars

let make_compiler_input (o : Ac.Output.t) (file_type : Ac.File_type.t)
    (user_cvars : Ac.C_variables.Map.t option)
    (config_fn : c_variables:Ac.C_variables.Map.t option -> 'cfg)
    (passes : Set.M(Act_sanitiser.Pass_group).t)
    (dl_output : Act_c.Filters.Output.t Filter_intf.chain_output) :
    'cfg Act_asm.Job.t Act_config.Compiler.Chain_input.t =
  let c_variables = choose_cvars o user_cvars dl_output in
  let symbols =
    c_variables
    |> Option.map ~f:Ac.C_id.Map.keys
    |> Option.map ~f:(List.map ~f:Ac.C_id.to_string)
  in
  let config = config_fn ~c_variables in
  let litmus_job = Act_asm.Job.make ~passes ~config ?symbols () in
  Act_config.Compiler.Chain_input.make
    ~file_type:(Act_common.File_type.delitmusified file_type)
    ~next:(Fn.const litmus_job)

let make_output_from_standard_args (args : Args.Standard.t) : Ac.Output.t =
  Ac.Output.make
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
      ~(f : B.t -> Ac.Output.t -> Act_config.Act.t -> unit Or_error.t)
      (args : B.t) : unit =
    let standard_args = B.as_standard_args args in
    setup_colour standard_args ;
    let o = make_output_from_standard_args standard_args in
    Or_error.(
      Args.Standard.config_file standard_args
      |> Io.fpath_of_string
      >>= Language_support.load_and_process_config ?compiler_predicate
            ?machine_predicate ?sanitiser_passes ?with_compiler_tests
      >>= f args o)
    |> Ac.Output.print_error o
end

module Standard_lifter = Make_lifter (Args.Standard)

let lift_command = Standard_lifter.lift

module With_files_lifter = Make_lifter (Args.Standard_with_files)

let lift_command_with_files = With_files_lifter.lift

module Asm_lifter = Make_lifter (Args.Standard_asm)

let lift_asm_command_basic
    ~(f :
          Args.Standard_asm.t
       -> Ac.Output.t
       -> Act_config.Act.t
       -> unit Or_error.t) (args : Args.Standard_asm.t) : unit =
  Asm_lifter.lift
    ?sanitiser_passes:(Args.Standard_asm.sanitiser_passes args)
    ~f args
