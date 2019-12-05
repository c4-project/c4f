(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module Pb = Plumbing
  module Tx = Travesty_base_exts
end

let no_make_harness (_spec : Spec.t) ~(arch : Arch.t) :
    Capability.Make_harness.t =
  ignore (arch : Arch.t) ;
  Cannot_make_harness
    {why= Error.of_string "This backend doesn't support harness making."}

module Make (B : Instance_types.Basic_with_run_info) : Instance_types.S =
struct
  module Reader = B.Reader

  let spec : Spec.t = Act_common.Spec.With_id.spec B.spec

  module Filter = Pb.Filter.Make_on_runner (struct
    module Runner = B.Runner

    type aux_i = Arch.t

    let name = Act_common.Id.to_string (Act_common.Spec.With_id.id B.spec)

    let prog (_arch : Arch.t) : string = Spec.cmd spec

    let argv (arch : Arch.t) (input_file : string) : string list Or_error.t =
      match B.run spec ~arch with
      | Cannot_run {why} ->
          Result.Error why
      | Can_run {argv_f} ->
          argv_f ~input_file
  end)

  let lift_make_harness_argv_f
      (argv_f :
        input_file:string -> output_dir:string -> string list Or_error.t) :
      Pb.Copy_projection.t Pb.Copy_spec.Pair.t -> string list Or_error.t =
    function
    | {input= Files [input_file_p]; output= Directory output_dir_p} ->
        let input_file = Pb.Copy_projection.remote input_file_p in
        let output_dir = Pb.Copy_projection.remote output_dir_p in
        argv_f ~input_file ~output_dir
    | copy_spec ->
        Or_error.error_s
          [%message
            "Can't use this copy-spec to make a harness"
              ~copy_spec:
                (copy_spec : Pb.Copy_projection.t Pb.Copy_spec.Pair.t)]

  let make_harness (arch : Arch.t) ~(input_file : Fpath.t)
      ~(output_dir : Fpath.t) : string list Or_error.t =
    match B.make_harness spec ~arch with
    | Capability.Make_harness.Cannot_make_harness {why} ->
        Result.Error why
    | Can_make_harness {argv_f; run_as} ->
        let prog = Spec.cmd spec in
        let argv_f = lift_make_harness_argv_f argv_f in
        let copy_specs =
          { Pb.Copy_spec.Pair.input= Pb.Copy_spec.file input_file
          ; output= Pb.Copy_spec.directory output_dir }
        in
        Tx.Or_error.(
          B.Runner.run_with_copy ~prog ~argv_f copy_specs
          >> Or_error.return run_as)

  let run (arch : Arch.t) ~(input_file : Fpath.t) ~(output_file : Fpath.t) :
      Act_state.Observation.t Or_error.t =
    Or_error.Let_syntax.(
      let%bind () =
        Filter.run arch
          (Pb.Input.of_fpath input_file)
          (Pb.Output.of_fpath output_file)
      in
      B.Reader.load (Pb.Input.of_fpath output_file))
end
