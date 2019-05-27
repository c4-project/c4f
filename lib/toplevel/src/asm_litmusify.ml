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
module A = Act_common
module Tx = Travesty_core_kernel_exts

let parse_post :
       [`Exists of Sexp.t]
    -> Sexp.t Act_litmus.Ast_base.Postcondition.t Or_error.t = function
  | `Exists sexp ->
      Or_error.try_with (fun () ->
          Act_litmus.Ast_base.Postcondition.make ~quantifier:`Exists
            ~predicate:([%of_sexp: Sexp.t Act_litmus.Ast_base.Pred.t] sexp)
      )

let make_litmus_config_fn (post_sexp : [`Exists of Sexp.t] option) :
    (   c_variables:A.C_variables.Map.t option
     -> Sexp.t Act_asm.Litmusifier.Config.t)
    Or_error.t =
  let open Or_error.Let_syntax in
  let%map postcondition =
    Tx.Option.With_errors.map_m post_sexp ~f:parse_post
  in
  fun ~c_variables ->
    Act_asm.Litmusifier.Config.make ?postcondition ?c_variables ()

module In = Asm_common.Input

let run (post_sexp : [`Exists of Sexp.t] option) (input : In.t) :
    unit Or_error.t =
  let cfg = In.act_config input in
  let infile = In.pb_input input in
  let outfile = In.pb_output input in
  let target = In.target input in
  let file_type = In.file_type input in
  let module R = Sim_support.Make_resolver (struct
    let cfg = cfg
  end) in
  Or_error.Let_syntax.(
    let%bind (module Filter) = Common.litmusify_pipeline target in
    let%bind litmus_cfg_fn = make_litmus_config_fn post_sexp in
    let job_input = In.make_compiler_input input litmus_cfg_fn in
    Or_error.ignore_m
      (Filter.run
         (Act_asm.Pipeline.Input.make ~file_type ~job_input)
         infile outfile))

let command =
  Command.basic ~summary:"converts an assembly file to a litmus test"
    Command.Let_syntax.(
      let%map_open standard_args = Args.Standard_asm.get
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
        Asm_common.lift_command standard_args ~f:(run post_sexp)
          ~default_passes:Act_sanitiser.Pass_group.standard)
