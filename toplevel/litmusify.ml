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
open Act_common

module Post_filter = struct
  module Cfg = struct
    type t = No_filter | Filter of {id: Id.t; arch: Sim.Arch.t}

    let arch_of_t : t -> Sim.Arch.t Or_error.t = function
      | Filter {arch; _} ->
          Or_error.return arch
      | No_filter ->
          Or_error.error_string
            "Tried to filter despite filtering being disabled"

    let make_filter (arch : Sim.Arch.t) (id : Id.t) = Filter {id; arch}

    let make ?(simulator : Id.t option) ~(arch : Sim.Arch.t) : t =
      Option.value_map simulator ~default:No_filter ~f:(make_filter arch)
  end

  module type S = Filter.S with type aux_i = Cfg.t and type aux_o = unit

  (** Adapts a simulator filter to try extract its per-run config from a
      [cfg]. *)
  module Adapt (S : Sim.Runner.Basic_filter) : S = Filter.Adapt (struct
    module Original = S

    type aux_i = Cfg.t

    type aux_o = unit

    let adapt_i : Cfg.t -> Sim.Arch.t Or_error.t = Cfg.arch_of_t

    let adapt_o : unit -> unit Or_error.t = Or_error.return
  end)

  (** This mainly exists so that there's still a module returned by
      {{!make} make}, even if we don't need one.

      In practice, {{!chain} chain} makes sure that this filter is never
      run. *)
  module Make_dummy (B : sig
    val error : Error.t
  end) : S = Filter.Make (struct
    type aux_i = Cfg.t

    type aux_o = unit

    let name = "dummy"

    let tmp_file_ext = Fn.const "tmp"

    let run (_ : _ Filter.ctx) (_ : In_channel.t) (_ : Out_channel.t) :
        unit Or_error.t =
      Result.Error B.error
  end)

  module No_filter_dummy : S = Make_dummy (struct
    let error =
      Error.of_string "Tried to use a filter when none was requested"
  end)

  module Missing_filter_dummy : S = Make_dummy (struct
    let error = Error.of_string "Tried to use a filter that doesn't exist"
  end)

  (** [make mtab] makes a post-filter module based on the request [which]. *)
  let make (mtab : (Id.t, (module S)) List.Assoc.t) : Cfg.t -> (module S) =
    function
    | No_filter ->
        (module No_filter_dummy)
    | Filter {id; _} ->
        id
        |> List.Assoc.find ~equal:Id.equal mtab
        |> Option.value ~default:(module Missing_filter_dummy : S)

  let chain (type i o) (filter : Cfg.t)
      (mtab : (Id.t, (module S)) List.Assoc.t)
      (module M : Filter.S with type aux_i = i and type aux_o = o) :
      (module Filter.S
         with type aux_i = i * (o Filter.chain_output -> Cfg.t)
          and type aux_o = o * unit option) =
    let (module Post) = make mtab filter in
    ( module Filter.Chain_conditional_second (struct
      type aux_i = i * (o Filter.chain_output -> Cfg.t)

      let select {Filter.aux= rest, cfg; _} =
        match cfg `Checking_ahead with
        | Cfg.No_filter ->
            `One rest
        | Cfg.Filter _ ->
            `Both (rest, cfg)

      module First = M
      module Second = Post
    end) )

  let get_arch (target : Config.Compiler.Target.t) : Sim.Arch.t =
    Assembly (Config.Compiler.Target.arch target)

  let machine_of_target (cfg : Config.Act.t) :
      Config.Compiler.Target.t -> Config.Machine.Spec.With_id.t Or_error.t =
    function
    | `Spec s ->
        Or_error.return (Config.Compiler.Spec.With_id.machine s)
    | `Arch _ ->
        (* TODO(@MattWindsor91): should really check that the machine has
           the right architecture! *)
        Config.Machine.Spec.Set.get (Config.Act.machines cfg)
          Config.Machine.Id.default

  let litmus_config (cfg : Config.Act.t) (target : Config.Compiler.Target.t)
      : Config.Litmus_tool.t Or_error.t =
    let open Or_error.Let_syntax in
    let%bind machine = machine_of_target cfg target in
    Or_error.tag_arg
      (Option.one (Config.Machine.Spec.With_id.litmus machine))
      "While trying to find litmus config for machine"
      (Config.Machine.Spec.With_id.id machine)
      [%sexp_of: Config.Machine.Id.t]

  let try_make_litmus_filter (cfg : Config.Act.t)
      (target : Config.Compiler.Target.t) : (module S) Or_error.t =
    Or_error.Let_syntax.(
      let%bind litmus_cfg = litmus_config cfg target in
      let%map mach = machine_of_target cfg target in
      (* TODO: redundant *)
      let (module R) = Config.Machine.Spec.With_id.runner mach in
      ( module Adapt (Sim_litmus.Filter.Make (struct
        let config = litmus_cfg

        module Runner = R
      end))
      : S ))

  let make_herd_filter (cfg : Config.Act.t) : (module S) =
    let cfg = Config.Act.herd_or_default cfg in
    ( module Adapt (Sim_herd.Filter.Make (struct
      let config = cfg
    end)) )

  let make_litmus_filter (cfg : Config.Act.t)
      (target : Config.Compiler.Target.t) : (module S) =
    match try_make_litmus_filter cfg target with
    | Ok m ->
        m
    | Error e ->
        ( module Make_dummy (struct
          let error = e
        end) )

  let make_mtab (cfg : Config.Act.t) (target : Config.Compiler.Target.t) :
      (Id.t, (module S)) List.Assoc.t =
    [ (Id.of_string "herd", make_herd_filter cfg)
    ; (Id.of_string "litmus", make_litmus_filter cfg target) ]
end

let make_filter (filter : Post_filter.Cfg.t)
    (target : Config.Compiler.Target.t)
    (mtab : (Id.t, (module Post_filter.S)) List.Assoc.t) :
    (module Filter.S
       with type aux_i = ( Config.File_type.t_or_infer
                         * (   C.Filters.Output.t Filter.chain_output
                            -> Sexp.t Litmusifier.Config.t Asm_job.t
                               Config.Compiler.Chain_input.t) )
                         * (   ( C.Filters.Output.t option
                               * (unit option * Asm_job.Output.t) )
                               Filter.chain_output
                            -> Post_filter.Cfg.t)
        and type aux_o = ( C.Filters.Output.t option
                         * (unit option * Asm_job.Output.t) )
                         * unit option)
    Or_error.t =
  let open Or_error in
  Common.(target |> litmusify_pipeline >>| Post_filter.chain filter mtab)

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

let run file_type (simulator : Id.t option) compiler_id_or_emits
    (c_globals : string list option) (c_locals : string list option)
    (post_sexp : [`Exists of Sexp.t] option)
    (args : Args.Standard_with_files.t) (o : Output.t) (cfg : Config.Act.t)
    : unit Or_error.t =
  let open Result.Let_syntax in
  let%bind target = Common.get_target cfg compiler_id_or_emits in
  let arch = Post_filter.get_arch target in
  let filter = Post_filter.Cfg.make ?simulator ~arch in
  let mtab = Post_filter.make_mtab cfg target in
  let%bind (module Filter) = make_filter filter target mtab in
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
      ((file_type, compiler_input_fn), Fn.const filter)
      ~infile:(Args.Standard_with_files.infile_raw args)
      ~outfile:(Args.Standard_with_files.outfile_raw args)
  in
  ()

let filter_arg = Command.Arg_type.create Id.of_string

let command =
  Command.basic ~summary:"converts an assembly file to a litmus test"
    Command.Let_syntax.(
      let%map_open standard_args = Args.Standard_with_files.get
      and sanitiser_passes = Args.sanitiser_passes
      and simulator =
        flag "simulator" (optional filter_arg)
          ~doc:"ID identifier of simulator to use, if any"
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
            (run file_type simulator compiler_id_or_arch c_globals c_locals
               post_sexp))
