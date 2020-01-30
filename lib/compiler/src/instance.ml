(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module Ac = Act_common
  module Au = Act_utils
  module Pb = Plumbing
  module Tx = Travesty_base_exts
end

let probe (module Runner : Pb.Runner_types.S)
    (module B : Instance_types.Basic) (prog : string) :
    Act_common.Id.t Or_error.t =
  Or_error.Let_syntax.(
    let%bind result = Runner.run_to_string ~prog B.probe_args in
    B.emits_of_probe result)

module Make (B : Instance_types.Basic_with_run_info) : Instance_types.S =
struct
  include B

  let cmd = Spec.With_id.cmd B.spec

  let is_c_input : string -> bool =
    (* TODO(@MattWindsor91): this may be overly naive. *)
    Act_utils.My_string.has_suffix ~suffix:".c"

  let sift_c_inputs :
      string Pb.Copy_spec.t -> string Au.Non_empty.t Or_error.t = function
    | Directory dir ->
        Or_error.error_s
          [%message "Expected at least one .c file, got a directory" ~dir]
    | Files fs ->
        fs |> List.filter ~f:is_c_input |> Au.Non_empty.of_list_err
        |> Or_error.tag ~tag:"Expected at least one .c file"
    | Nothing ->
        Or_error.error_string "Expected at least one .c file, got nothing"

  let sift_output : string Pb.Copy_spec.t -> string Or_error.t = function
    | Directory dir ->
        Or_error.error_s
          [%message "Expected one output file, got a directory" ~dir]
    | Nothing ->
        Or_error.error_string "Expected at one output file, got nothing"
    | Files fs ->
        fs |> Tx.List.one |> Or_error.tag ~tag:"Expected one output file"

  let make_argv (spec : Spec.t) (mode : Mode.t)
      (c_specs : Pb.Copy_projection.t Pb.Copy_spec.Pair.t) :
      string list Or_error.t =
    let user_args = Spec.argv spec in
    let arch = Spec.emits spec in
    let {Pb.Copy_spec.Pair.input; output} =
      Pb.Copy_projection.all_remote_pair c_specs
    in
    Or_error.Let_syntax.(
      let%bind infiles = sift_c_inputs input in
      let%map outfile = sift_output output in
      compile_args ~user_args ~arch ~mode
        ~infiles:(Au.Non_empty.to_list infiles)
        ~outfile)

  let check_mode_compatible (mode : Mode.t) (infiles : Fpath.t list) =
    match infiles with
    | _ :: _ :: _ when not (Mode.supports_multiple_inputs mode) ->
        Or_error.error_s
          [%message
            "Mode does not support multiple inputs." ~mode:(mode : Mode.t)]
    | _ ->
        Ok ()

  let compile (mode : Mode.t) ~(infiles : Fpath.t list) ~(outfile : Fpath.t)
      : unit Or_error.t =
    let spec = Spec.With_id.spec B.spec in
    Or_error.Let_syntax.(
      let%bind () = check_mode_compatible mode infiles in
      B.Runner.run_with_copy ~prog:cmd
        {input= Pb.Copy_spec.files infiles; output= Pb.Copy_spec.file outfile}
        ~argv_f:(make_argv spec mode))

  let probe () : Act_common.Id.t Or_error.t =
    probe (module Runner) (module B) cmd
end
