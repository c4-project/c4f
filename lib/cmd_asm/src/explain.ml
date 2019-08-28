(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core_kernel
module A = Act_common

let print_symbol_map = function
  | [] ->
      ()
  | map ->
      Fmt.(
        pr "@[<v>@,Symbol map:@,@,%a@]@."
          (list ~sep:sp (fun f (k, v) -> pf f "@[<hv>%s@ ->@ %s@]" k v))
          map)

let explain_runner (module B : Act_asm.Runner_intf.Basic) :
    (module Act_asm.Explainer.S_filter) =
  let module Exp = Act_asm.Explainer.Make (B) in
  (module Exp.Filter)

let explain_filter (target : Act_machine.Qualified.Compiler.t Act_machine.Target.t) :
    (module Act_asm.Explainer.S_filter) Or_error.t =
  Or_error.(
    tag ~tag:"while getting an explain filter for this target"
      (target |> Toplevel.Language_support.asm_runner_of_target >>| explain_runner))

let run_with_input (o : A.Output.t) target job_input infile outfile =
  Or_error.Let_syntax.(
    let%bind (module Exp) = explain_filter target in
    A.Output.pv o "Got explain filter (name %s)" Exp.name ;
    Exp.run job_input infile outfile)

module In = Common.Input

let run output_format (input : In.t) =
  let o = In.output input in
  let infile = In.pb_input input in
  let outfile = In.pb_output input in
  let target = In.target input in
  Or_error.Let_syntax.(
    let explain_cfg _c_variables =
      Act_asm.Explainer.Config.make ?format:output_format ()
    in
    let job_input = In.make_job_input input explain_cfg in
    A.Output.pv o "About to get and run the explain filter.@." ;
    let%map out = run_with_input o target job_input infile outfile in
    A.Output.pw o "@[%a@]@." Act_asm.Job.Output.warn out ;
    print_symbol_map (Act_asm.Job.Output.symbol_map out))

let command =
  Command.basic ~summary:"explains act's understanding of an assembly file"
    Command.Let_syntax.(
      let%map_open standard_args = ignore anon ; Args.Standard_asm.get
      and output_format =
        Act_asm.Explainer.Config.Format.(
          choose_one
            [ map
                ~f:(fun flag -> Option.some_if flag (Some Detailed))
                (flag "detailed" no_arg
                   ~doc:"Print a detailed (but long-winded) explanation")
            ; map
                ~f:(fun flag -> Option.some_if flag (Some Assembly))
                (flag "as-assembly" no_arg
                   ~doc:"Print explanation as lightly annotated assembly")
            ]
            ~if_nothing_chosen:(`Default_to None))
      in
      fun () ->
        Common.lift_command standard_args ~f:(run output_format)
          ~default_passes:Act_sanitiser.Pass_group.explain)
