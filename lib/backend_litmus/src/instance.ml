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

let arch_map : (Act_common.Id.t, string) List.Assoc.t Lazy.t =
  lazy Act_common.Id.[
    (of_string "x86", "X86");
    (of_string "x64", "X86");
    (of_string "ppc64", "PPC")
  ]

let lookup_litmus_arch :
    Act_common.Id.t -> (Act_common.Id.t * string) Or_error.t =
  Act_common.Id.try_find_assoc_with_suggestions_prefix (Lazy.force arch_map)
    ~id_type:"architecture supported by Litmus7"

let make_arch_args : Bk.Arch.t -> string list Or_error.t = function
  | C {underlying_arch= None} ->
      Or_error.error_string
        "Must specify a target architecture when making a C harness"
  | C {underlying_arch= Some arch} ->
      Or_error.Let_syntax.(
        let%map _, litmus_arch = lookup_litmus_arch arch in
        ["-c11"; "true"; "-carch"; litmus_arch])
  | Assembly id ->
      (* for now *)
      ignore id ; Or_error.return []

let make_harness_argv (arch : Bk.Arch.t) :
    (input_file:string -> output_dir:string -> string list Or_error.t)
    Staged.t
    Or_error.t =
  Or_error.Let_syntax.(
    let%map arch_args = make_arch_args arch in
    Staged.stage (fun ~input_file ~output_dir ->
        Or_error.return ([input_file; "-o"; output_dir] @ arch_args)))

let make_harness (_spec : Bk.Spec.t) ~(arch : Bk.Arch.t) :
    Bk.Capability.Make_harness.t =
  match make_harness_argv arch with
  | Ok argv_f ->
      Can_make_harness
        {argv_f= Staged.unstage argv_f; run_as= ["make"; "sh ./run.sh"]}
  | Error why ->
      Cannot_make_harness {why}

let make_run_argv (arch : Bk.Arch.t) :
    (input_file:string -> string list Or_error.t) Staged.t Or_error.t =
  Or_error.Let_syntax.(
    let%map arch_args = make_arch_args arch in
    Staged.stage (fun ~input_file ->
        Or_error.return ([input_file] @ arch_args)))

let run (_spec : Bk.Spec.t) ~(arch : Bk.Arch.t) : Bk.Capability.Run.t =
  match make_run_argv arch with
  | Ok argv_f ->
      Can_run {argv_f= Staged.unstage argv_f}
  | Error why ->
      Cannot_run {why}
