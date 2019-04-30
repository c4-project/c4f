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

open Travesty_base_exts
open Lib
include Instance_intf

(** [Make_machine] makes a single-machine test runner from a
    [Basic_machine]. *)
module Make_machine (B : Basic_machine) : Machine = struct
  include B

  let make_pathset (cfg : Run_config.t)
      (spec : Config.Compiler.Spec.With_id.t) : Pathset.t Or_error.t =
    let open Or_error.Let_syntax in
    let id = Config.Compiler.Spec.With_id.id spec in
    let output_root = Run_config.output_root cfg in
    let input_mode = Run_config.input_mode cfg in
    let%map ps = Pathset.make_and_mkdirs id ~output_root ~input_mode in
    Output.pv o "%a@." Pathset.pp ps ;
    ps

  let make_compiler (cfg : Run_config.t)
      (spec : Config.Compiler.Spec.With_id.t) :
      (module Compiler.S) Or_error.t =
    let open Or_error.Let_syntax in
    let%bind (module C) = B.Resolve_compiler.from_spec spec in
    let%bind (module R) = B.asm_runner_from_spec spec in
    let%map ps = make_pathset cfg spec in
    ( module Compiler.Make (struct
      include (B : Basic)

      module C = C
      module R = R

      let ps = ps

      let cspec = spec
    end)
    : Compiler.S )

  let run_compiler (cfg : Run_config.t)
      (spec : Config.Compiler.Spec.With_id.t) =
    let open Or_error.Let_syntax in
    let id = Config.Compiler.Spec.With_id.id spec in
    let%bind (module TC) = make_compiler cfg spec in
    let%map result = TC.run () in
    (id, result)

  let run_compilers (cfg : Run_config.t) :
      (Config.Id.t, Analysis.Compiler.t) List.Assoc.t Or_error.t =
    compilers
    |> Config.Compiler.Spec.Set.map ~f:(run_compiler cfg)
    |> Or_error.combine_errors

  let make_analysis
      (raw : (Config.Id.t, Analysis.Compiler.t) List.Assoc.t T.t) :
      Analysis.Machine.t =
    Analysis.Machine.make ~compilers:(T.value raw)
      ?time_taken:(T.time_taken raw) ()

  let run (cfg : Run_config.t) : Analysis.Machine.t Or_error.t =
    let open Or_error.Let_syntax in
    let%map compilers_and_time =
      T.bracket_join (fun () -> run_compilers cfg)
    in
    make_analysis compilers_and_time
end

module Machine_assoc = Alist.Fix_left (Config.Machine.Id)

(** Compiler specification sets, grouped by machine. *)
module Compiler_spec_env = struct
  include Machine_assoc.Fix_right (Config.Compiler.Spec.Set)

  let group_by_machine specs =
    specs
    |> Config.Compiler.Spec.Set.group ~f:(fun spec ->
           Config.Machine.Spec.With_id.id
             (Config.Compiler.Spec.With_id.machine spec) )
    |> Config.Id.Map.to_alist

  let get (cfg : Run_config.t) (compilers : Config.Compiler.Spec.Set.t) : t
      =
    let enabled_ids = Run_config.compilers cfg in
    let specs = Config.Compiler.Spec.Set.restrict compilers enabled_ids in
    group_by_machine specs
end

(** Bulding of everything used in a tester instance. *)

(*module Ctx = struct type t = { config: Run_config.t ; specs:
  Compiler_spec_env.t } [@@deriving fields, make] end*)

module Make (B : Basic) : S = struct
  include B

  let make_analysis
      (raw : (Config.Id.t, Analysis.Machine.t) List.Assoc.t T.t) :
      Analysis.t =
    Analysis.make ~machines:(T.value raw) ?time_taken:(T.time_taken raw) ()

  let make_machine_module (mach_compilers : Config.Compiler.Spec.Set.t) =
    ( module Make_machine (struct
      include B

      (* Reduce the set of compilers to those specifically used in this
         machine. *)
      let compilers = mach_compilers
    end)
    : Machine )

  let run_machine (tester_cfg : Run_config.t) (mach_id, mach_compilers) =
    let open Or_error.Let_syntax in
    let (module TM) = make_machine_module mach_compilers in
    let%map analysis = TM.run tester_cfg in
    (mach_id, analysis)

  let run_machines (cfg : Run_config.t)
      (specs_by_machine : Config.Compiler.Spec.Set.t Machine_assoc.t) :
      Analysis.Machine.t Machine_assoc.t Or_error.t =
    specs_by_machine
    |> List.map ~f:(run_machine cfg)
    |> Or_error.combine_errors

  let run_with_specs (cfg : Run_config.t) (specs : Compiler_spec_env.t) :
      Analysis.t Or_error.t =
    let open Or_error.Let_syntax in
    let%map machines_and_time =
      T.bracket_join (fun () -> run_machines cfg specs)
    in
    make_analysis machines_and_time

  let run (cfg : Run_config.t) : Analysis.t Or_error.t =
    run_with_specs cfg (Compiler_spec_env.get cfg compilers)
end
