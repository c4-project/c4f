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
include Machine_intf

module Make (B : Basic) : S = struct
  include B
  include Common.Extend (B)

  let make_pathset (cfg : Run_config.t)
      (spec : Config.Compiler.Spec.With_id.t) :
      Pathset.Compiler.t Or_error.t =
    let open Or_error.Let_syntax in
    let compiler_id = Config.Compiler.Spec.With_id.id spec in
    let%map ps =
      Pathset.Compiler.make_and_mkdirs
        {compiler_id; run= Run_config.pathset cfg}
    in
    Output.pv o "%a@." Pathset.Compiler.pp ps ;
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

  let run_compiler (cfg : Run_config.t) (c_sims : Sim.File_map.t)
      (spec : Config.Compiler.Spec.With_id.t) =
    let open Or_error.Let_syntax in
    let id = Config.Compiler.Spec.With_id.id spec in
    let%bind (module TC) = make_compiler cfg spec in
    let%map result = TC.run c_sims in
    (id, result)

  let run_compilers (cfg : Run_config.t)
      (c_sims : Sim.File_map.t) :
      (Config.Id.t, Analysis.Compiler.t) List.Assoc.t Or_error.t =
    compilers
    |> Config.Compiler.Spec.Set.map ~f:(run_compiler cfg c_sims)
    |> Or_error.combine_errors

  let make_analysis
      (raw : (Config.Id.t, Analysis.Compiler.t) List.Assoc.t T.t) :
      Analysis.Machine.t =
    Analysis.Machine.make ~compilers:(T.value raw)
      ?time_taken:(T.time_taken raw) ()

  let run (cfg : Run_config.t) (c_sims : Sim.File_map.t) :
      Analysis.Machine.t Or_error.t =
    let open Or_error.Let_syntax in
    let%map compilers_and_time =
      T.bracket_join (fun () -> run_compilers cfg c_sims)
    in
    make_analysis compilers_and_time
end
