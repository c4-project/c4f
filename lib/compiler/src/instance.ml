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

  let make_argv_fun (spec : Spec.t) (mode : Mode.t) =
    let user_args = Spec.argv spec in
    let arch = Spec.emits spec in
    Pb.Runner.argv_one_file (fun ~input ~output ->
        Or_error.return
          (B.compile_args ~user_args ~arch ~mode ~infile:input
             ~outfile:output))

  let compile (mode : Mode.t) ~(infile : Fpath.t) ~(outfile : Fpath.t) =
    let spec = Spec.With_id.spec B.cspec in
    B.Runner.run_with_copy ~prog:cmd
      {input= Pb.Copy_spec.file infile; output= Pb.Copy_spec.file outfile}
      (make_argv_fun spec mode)

  let test () = B.Runner.run ~prog:cmd B.test_args
end

module Fail (E : sig
  val error : Error.t
end) : Instance_types.S = struct
  let test () = Result.ok_unit

  let compile (_mode : Mode.t) ~infile ~outfile =
    ignore infile ; ignore outfile ; Result.Error E.error
end
