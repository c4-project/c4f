(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Ac = Act_common
module Au = Act_utils
module Pb = Plumbing

module Make (B : Instance_types.Basic_with_run_info) : Instance_types.S =
struct
  include B

  let cmd = Spec.With_id.cmd B.cspec

  let make_argv (spec : Spec.t) (mode : Mode.t)
      ~(input : string Pb.Copy_spec.t) ~(output : string Pb.Copy_spec.t) :
      string list Or_error.t =
    let user_args = Spec.argv spec in
    let arch = Spec.emits spec in
    match (input, output) with
    | Files infiles, Files [outfile] ->
        Or_error.return
          (compile_args ~user_args ~arch ~mode ~infiles ~outfile)
    | _, _ ->
        Or_error.error_string
          "Expected one output file and at least one output file"

  let check_mode_compatible (mode : Mode.t) (infiles : Fpath.t list) =
    match infiles with
    | _ :: _ :: _ when not (Mode.supports_multiple_inputs mode) ->
        Or_error.error_s
          [%message
            "Mode does not support multiple inputs." ~mode:(mode : Mode.t)]
    | _ ->
        Ok ()

  let compile (mode : Mode.t) ~(infiles : Fpath.t list) ~(outfile : Fpath.t)
      =
    let spec = Spec.With_id.spec B.cspec in
    Or_error.Let_syntax.(
      let%bind () = check_mode_compatible mode infiles in
      B.Runner.run_with_copy ~prog:cmd
        {input= Pb.Copy_spec.files infiles; output= Pb.Copy_spec.file outfile}
        (make_argv spec mode))

  let test () = B.Runner.run ~prog:cmd B.test_args
end

module Fail (E : sig
  val error : Error.t
end) : Instance_types.S = struct
  let test () = Ok ()

  let compile (_mode : Mode.t) ~(infiles : Fpath.t list) ~(outfile : Fpath.t)
      =
    ignore infiles ; ignore outfile ; Result.Error E.error
end
