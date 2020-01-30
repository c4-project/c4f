(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

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

let binary_names : string Sequence.t = Sequence.of_list ["herd7"; "herd"]

let make_harness :
    Bk.Spec.t -> arch:Bk.Arch.t -> Bk.Capability.Make_harness.t =
  Bk.Instance.no_make_harness

let probe_args : string array array = [|[|"-version"|]|]

let model_for_arch (config : Act_backend.Spec.t) :
    Act_backend.Arch.t -> string option Or_error.t = function
  | C _ ->
      Or_error.return (Act_backend.Spec.c_model config)
  | Assembly emits_spec ->
      Or_error.return
        (List.Assoc.find
           (Act_backend.Spec.asm_models config)
           emits_spec ~equal:[%equal: Act_common.Id.t])

let capabilities ~(test_stdout : string list) : Bk.Capability.Summary.t =
  ignore (test_stdout : string list) ;
  (* TODO(@MattWindsor91): scrape stuff from test_stdout *)
  Bk.Capability.Summary.make
    ~flags:(Set.of_list (module Bk.Capability.Flag) Bk.Capability.Flag.[Run])
    ~arches:(Set.of_list (module Bk.Arch) Bk.Arch.[c; asm_x86])

let make_argv_head ?(model : string option) () : string list =
  Option.value_map model ~f:(fun m -> ["-model"; m]) ~default:[]

let make_argv_from_spec (spec : Act_backend.Spec.t)
    (arch : Act_backend.Arch.t) ~(input_file : string) :
    string list Or_error.t =
  Or_error.Let_syntax.(
    let%map model = model_for_arch spec arch in
    make_argv_head ?model () @ [input_file])

let run (spec : Bk.Spec.t) ~(arch : Bk.Arch.t) : Bk.Capability.Run.t =
  let argv_f = make_argv_from_spec spec arch in
  Bk.Capability.Run.Can_run {argv_f}
