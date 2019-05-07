(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

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
open Travesty_core_kernel_exts
open Lib
open Utils

(* TODO(@MattWindsor91): clean this up. *)
module type Herd_in = Filter.S with type aux_i = Sim.Arch.t and type aux_o = unit

module Post_filter = struct
  module Err = Or_error

  type t = [`Herd | `Litmus | `None]

  type cfg = [`Herd of Sim.Arch.t | `Litmus of Config.Litmus_tool.t | `None]

  (** All post-filters have the same signature. *)
  module type S = Filter.S with type aux_i = cfg and type aux_o = unit


  (** Adapts a Herd filter to try extract its per-run config from a [cfg]. *)
  module Herd_filter (H : Herd_in) : S = Filter.Adapt (struct
    module Original = H

    type aux_i = cfg
    type aux_o = unit

    let adapt_i : cfg -> Sim.Arch.t Err.t = function
      | `Herd arch -> Err.return arch
      | `Litmus _ ->
          Err.error_string "Expected Herd config, got litmus"
      | `None ->
          Err.error_string "Expected Herd config, got none"

    let adapt_o : unit -> unit Err.t = Err.return
  end)

  (** Adapts the Litmus-tool filter to try extract its config from a [cfg]. *)
  module Litmus_filter (R : Runner.S) : S = Filter.Adapt (struct
    module Original = Litmus_tool.Filter (R)

    type aux_i = cfg

    type aux_o = unit

    let adapt_i : cfg -> Config.Litmus_tool.t Err.t = function
      | `Litmus l ->
          Err.return l
      | `Herd _ ->
          Err.error_string "Expected Litmus config, got herd"
      | `None ->
          Err.error_string "Expected Litmus config, got none"

    let adapt_o : unit -> unit Err.t = Err.return
  end)

  (** This mainly exists so that there's still a module returned by
      {{!make} make}, even if we don't need one.

      In practice, {{!chain} chain} makes sure that this filter is never
      run. *)
  module Dummy : S = Filter.Make (struct
    type aux_i = cfg

    type aux_o = unit

    let name = "dummy"

    let tmp_file_ext = Fn.const "tmp"

    let run (_ : cfg Filter.ctx) (_ : In_channel.t) (_ : Out_channel.t) :
        unit Err.t =
      Err.error_string
        "Internal error: tried to postprocess litmus with no filter"
  end)

  (** [make herd_cfg mach_runner which] makes a post-filter module based on the
      request [which]. If the post-filter is Litmus, then it is set up to
      run using [mach_runner]; other post-filters are run locally. *)
  let make (module H : Herd_in) (module Mach_runner : Runner.S) : t -> (module S) = function
    | `Herd ->
        (module Herd_filter (H))
    | `Litmus ->
        (module Litmus_filter (Mach_runner))
    | `None ->
        (module Dummy)

  let chain (type i o) (filter : t) (module H : Herd_in) (module R : Runner.S)
      (module M : Filter.S with type aux_i = i and type aux_o = o) :
      (module Filter.S
         with type aux_i = i * (o Filter.chain_output -> cfg)
          and type aux_o = o * unit option) =
    let (module Post) = make (module H) (module R) filter in
    ( module Filter.Chain_conditional_second (struct
      type aux_i = i * (o Filter.chain_output -> cfg)

      let select {Filter.aux= rest, cfg; _} =
        match cfg `Checking_ahead with
        | `None ->
            `One rest
        | `Herd _ | `Litmus _ ->
            `Both (rest, cfg)

      module First = M
      module Second = Post
    end) )

  let get_arch (target : Config.Compiler.Target.t) :
    Sim.Arch.t =
    Assembly (Config.Compiler.Target.arch target)

  let machine_of_target (cfg : Config.Act.t) :
      Config.Compiler.Target.t -> Config.Machine.Spec.With_id.t Err.t =
    function
    | `Spec s ->
        Err.return (Config.Compiler.Spec.With_id.machine s)
    | `Arch _ ->
        (* TODO(@MattWindsor91): should really check that the machine has
           the right architecture! *)
        Config.Machine.Spec.Set.get (Config.Act.machines cfg)
          Config.Machine.Id.default

  let litmus_config (cfg : Config.Act.t) (target : Config.Compiler.Target.t)
      : Config.Litmus_tool.t Err.t =
    let open Err.Let_syntax in
    let%bind machine = machine_of_target cfg target in
    Err.tag_arg
      (Option.one (Config.Machine.Spec.With_id.litmus machine))
      "While trying to find litmus config for machine"
      (Config.Machine.Spec.With_id.id machine)
      [%sexp_of: Config.Machine.Id.t]

  let make_config (cfg : Config.Act.t) (target : Config.Compiler.Target.t) :
      t -> cfg Err.t = function
    | `Herd ->
        Err.return (`Herd (get_arch target))
    | `Litmus ->
        Err.(litmus_config cfg target >>| fun l -> `Litmus l)
    | `None ->
        Err.return `None
end

let make_filter (post_filter : Post_filter.t)
    (target : Config.Compiler.Target.t)
    (herd : (module Herd_in))
    (target_runner : (module Runner.S))
    :
    (module Filter.S
       with type aux_i = ( Config.File_type.t_or_infer
                         * (   C.Filters.Output.t Filter.chain_output
                            -> Sexp.t Litmusifier.Config.t Asm_job.t
                               Config.Compiler.Chain_input.t) )
                         * (   ( C.Filters.Output.t option
                               * (unit option * Asm_job.Output.t) )
                               Filter.chain_output
                            -> Post_filter.cfg)
        and type aux_o = ( C.Filters.Output.t option
                         * (unit option * Asm_job.Output.t) )
                         * unit option)
    Or_error.t =
  let open Or_error in
  Common.(
    target |> litmusify_pipeline
    >>| Post_filter.chain post_filter herd target_runner)

let parse_post :
    [`Exists of Sexp.t] -> Sexp.t Litmus.Ast_base.Postcondition.t Or_error.t
    = function
  | `Exists sexp ->
      Or_error.try_with (fun () ->
          Litmus.Ast_base.Postcondition.make ~quantifier:`Exists
            ~predicate:([%of_sexp: Sexp.t Litmus.Ast_base.Pred.t] sexp) )

let make_litmus_config_fn (post_sexp : [`Exists of Sexp.t] option) :
    (   c_variables:Config.C_variables.Map.t option
     -> Sexp.t Litmusifier.Config.t)
    Or_error.t =
  let open Or_error.Let_syntax in
  let%map postcondition =
    Option.With_errors.map_m post_sexp ~f:parse_post
  in
  fun ~c_variables -> Litmusifier.Config.make ?postcondition ?c_variables ()

(* TODO(@MattWindsor91): merge with tester logic *)
let make_herd_module (cfg : Config.Act.t) : (module Herd_in) =
  let herd_cfg = Config.Act.herd_or_default cfg in
  (module Sim_herd.Filter.Make (struct let config = herd_cfg end))

let run file_type (filter : Post_filter.t) compiler_id_or_emits
    (c_globals : string list option) (c_locals : string list option)
    (post_sexp : [`Exists of Sexp.t] option)
    (args : Args.Standard_with_files.t) (o : Output.t) (cfg : Config.Act.t)
    : unit Or_error.t =
  let open Result.Let_syntax in
  let%bind target = Common.get_target cfg compiler_id_or_emits in
  let herd = make_herd_module cfg in
  let%bind tgt_machine = Post_filter.machine_of_target cfg target in
  let tgt_runner = Config.Machine.Spec.With_id.runner tgt_machine in
  let%bind (module Filter) = make_filter filter target herd tgt_runner in
  let%bind pf_cfg = Post_filter.make_config cfg target filter in
  let passes =
    Config.Act.sanitiser_passes cfg ~default:Config.Sanitiser_pass.standard
  in
  let%bind litmus_cfg_fn = make_litmus_config_fn post_sexp in
  let%bind user_cvars = Common.collect_cvars ?c_globals ?c_locals () in
  let compiler_input_fn =
    Common.make_compiler_input o file_type user_cvars litmus_cfg_fn passes
  in
  let%map _ =
    Filter.run_from_string_paths
      ((file_type, compiler_input_fn), Fn.const pf_cfg)
      ~infile:(Args.Standard_with_files.infile_raw args)
      ~outfile:(Args.Standard_with_files.outfile_raw args)
  in
  ()

let command =
  Command.basic ~summary:"converts an assembly file to a litmus test"
    Command.Let_syntax.(
      let%map_open standard_args = Args.Standard_with_files.get
      and sanitiser_passes = Args.sanitiser_passes
      and filter =
        Args.(
          choose_one
            [ flag_to_enum_choice `Herd "herd"
                ~doc:"pipe results through `herd`"
            ; flag_to_enum_choice `Litmus "litmus"
                ~doc:"pipe results through `litmus`" ]
            ~if_nothing_chosen:(`Default_to `None))
      and file_type = Args.file_type
      and compiler_id_or_arch = Args.compiler_id_or_arch
      and c_globals = Args.c_globals
      and c_locals = Args.c_locals
      and post_sexp =
        choose_one
          [ map
              ~f:(Option.map ~f:(fun x -> Some (`Exists x)))
              (flag "exists"
                 (* We can't actually type-check the postcondition until we
                    have a target language! *)
                 (optional sexp)
                 ~doc:
                   "PREDICATE an 'exists' postcondition to attach to the \
                    resulting Litmus test") ]
          ~if_nothing_chosen:(`Default_to None)
      in
      fun () ->
        Common.lift_command_with_files standard_args ?sanitiser_passes
          ~with_compiler_tests:false
          ~f:
            (run file_type filter compiler_id_or_arch c_globals c_locals
               post_sexp))
