(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module Bk = Act_backend
end

(* Re-exported to match interface *)
module Reader = Reader

let test_args : string list = ["-version"]

let capabilities ~(test_stdout : string list) : Bk.Capability.Summary.t =
  ignore (test_stdout : string list) ;
  (* TODO(@MattWindsor91): scrape stuff from test_stdout *)
  Bk.Capability.Summary.make
    ~flags:
      (Set.of_list
         (module Bk.Capability.Flag)
         Bk.Capability.Flag.[Run; Make_harness])
    ~arches:(Set.of_list (module Bk.Arch) (* for now *) Bk.Arch.[c; asm_x86])

let make_harness_argv (_arch : Act_backend.Arch.t) ~(input_file : string)
    ~(output_dir : string) : string list Or_error.t =
  Or_error.return [input_file; "-o"; output_dir]

let make_harness (_spec : Bk.Spec.t) ~(arch : Bk.Arch.t) :
    Bk.Capability.Make_harness.t =
  (* for now *)
  match arch with
  | C ->
      Cannot_make_harness
        { why=
            Error.of_string
              "Must specify a target architecture when making a C harness" }
  | Assembly id ->
      ignore id ;
      Can_make_harness
        {argv_f= make_harness_argv arch; run_as= ["make"; "sh ./run.sh"]}

let make_argv_from_spec (_spec : Act_backend.Spec.t)
    (_arch : Act_backend.Arch.t) ~(input_file : string) :
    string list Or_error.t =
  Or_error.Let_syntax.((* for now *)
                       return [input_file])

let run (spec : Bk.Spec.t) ~(arch : Bk.Arch.t) : Bk.Capability.Run.t =
  let argv_f = make_argv_from_spec spec arch in
  Bk.Capability.Run.Can_run {argv_f}
