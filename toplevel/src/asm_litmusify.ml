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
module A = Act_common
module Tx = Travesty_core_kernel_exts

module Post_filter = struct
  module Cfg = struct
    type t = No_filter | Filter of {id: A.Id.t; arch: Sim.Arch.t}

    let arch_of_t : t -> Sim.Arch.t Or_error.t = function
      | Filter {arch; _} ->
          Or_error.return arch
      | No_filter ->
          Or_error.error_string
            "Tried to filter despite filtering being disabled"

    let make_filter (arch : Sim.Arch.t) (id : A.Id.t) = Filter {id; arch}

    let make ?(simulator : A.Id.t option) ~(arch : Sim.Arch.t) : t =
      Option.value_map simulator ~default:No_filter ~f:(make_filter arch)
  end

  module type S = Filter.S with type aux_i = Cfg.t and type aux_o = unit

  (** Adapts a simulator filter to try extract its per-run config from a
      [cfg]. *)
  module Adapt (S : Sim.Runner.S) : S = Filter.Adapt (struct
    module Original = S.Filter

    type aux_i = Cfg.t

    type aux_o = unit

    let adapt_i : Cfg.t -> Sim.Arch.t Or_error.t = Cfg.arch_of_t

    let adapt_o : unit -> unit Or_error.t = Or_error.return
  end)

  module Make_dummy (B : sig
    val error : Error.t
  end) : S =
    Adapt (Sim.Runner.Make_error (B))

  (** This mainly exists so that there's still a module returned by
      {{!make} make}, even if we don't need one.

      In practice, {{!chain} chain} makes sure that this filter is never
      run. *)
  module No_filter_dummy : S = Make_dummy (struct
    let error =
      Error.of_string "Tried to use a filter when none was requested"
  end)

  (** [make mtab] makes a post-filter module based on the request [which]. *)
  let make (mtab : Sim.Table.t) : Cfg.t -> (module S) = function
    | No_filter ->
        (module No_filter_dummy)
    | Filter {id; _} ->
        (module Adapt ((val Sim.Table.get mtab id)))

  let chain (type i o) (filter : Cfg.t) (mtab : Sim.Table.t)
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
end

let make_filter (filter : Post_filter.Cfg.t)
    (target : Config.Compiler.Target.t) (mtab : Sim.Table.t) :
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
    (c_variables:A.C_variables.Map.t option -> Sexp.t Litmusifier.Config.t)
    Or_error.t =
  let open Or_error.Let_syntax in
  let%map postcondition =
    Tx.Option.With_errors.map_m post_sexp ~f:parse_post
  in
  fun ~c_variables -> Litmusifier.Config.make ?postcondition ?c_variables ()

let get_arch (target : Config.Compiler.Target.t) : Sim.Arch.t =
  Assembly (Config.Compiler.Target.arch target)

let to_machine_id : Config.Compiler.Target.t -> Config.Machine.Id.t =
  function
  | `Spec s ->
    s |> Config.Compiler.Spec.With_id.machine
    |> Config.Machine.Spec.With_id.id
  | `Arch _ ->
    Config.Machine.Id.default

let run
    (simulator : A.Id.t option)
    (post_sexp : [`Exists of Sexp.t] option)
    (args : Args.Standard_asm.t) (o : A.Output.t)
    (cfg : Config.Act.t) : unit Or_error.t =
  let open Result.Let_syntax in
  let raw_target = Args.Standard_asm.target args in
  let%bind target = Asm_target.resolve ~cfg raw_target in
  let machine_id = to_machine_id target in
  let arch = get_arch target in
  let filter = Post_filter.Cfg.make ?simulator ~arch in
  let module R = Sim_support.Make_resolver (struct
    let cfg = cfg
  end) in
  let%bind mtab = R.make_table machine_id in
  let%bind (module Filter) = make_filter filter target mtab in
  let passes =
    Config.Act.sanitiser_passes cfg ~default:Config.Sanitiser_pass.standard
  in
  let%bind litmus_cfg_fn = make_litmus_config_fn post_sexp in
  let%bind user_cvars = Common.collect_cvars args in
  let file_type = Args.Standard_asm.file_type args in
  let compiler_input_fn =
    Common.make_compiler_input o file_type user_cvars litmus_cfg_fn passes
  in
  let%map _ =
    Filter.run_from_string_paths
      ((file_type, compiler_input_fn), Fn.const filter)
      ~infile:(Args.Standard_asm.infile_raw args)
      ~outfile:(Args.Standard_asm.outfile_raw args)
  in
  ()

let command =
  Command.basic ~summary:"converts an assembly file to a litmus test"
    Command.Let_syntax.(
      let%map_open standard_args = Args.Standard_asm.get
      and simulator = Args.simulator ()
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
        Common.lift_asm_command standard_args
          ~f:(run simulator post_sexp))
