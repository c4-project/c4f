(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

let mode_args : Act_compiler.Mode.t -> string list = function
  | Assembly ->
      (* -fno-pic disables position independent code, which is useful in
         assembly mode as it removes a lot of indirection and cruft that
         frustrates trying to use the output of a GCC-like compiler as an
         assembly corpus for other tools. It ISN'T so useful in other modes,
         and can break binary builds outright. *)
      ["-S"; "-fno-pic"]
  | Object ->
      ["-c"]
  | Binary ->
      []

let output_args (file : string) : string list = ["-o"; file]

let compile_args ~(user_args : string list) ~(arch : Act_common.Id.t)
    ~(mode : Act_compiler.Mode.t) ~(infiles : string list)
    ~(outfile : string) =
  ignore arch ;
  List.concat [mode_args mode; user_args; output_args outfile; infiles]

let probe_args = ["--version"]

let to_target_triplet (s : string) : (string * string * string) Or_error.t =
  match String.split (String.strip s) ~on:'-' with
  | [cpu; vendor; os] ->
      Ok (cpu, vendor, os)
  | _ ->
      Or_error.errorf "malformed target triplet: %s" s

let cpu_map : Act_common.Id.t Map.M(String).t Lazy.t =
  lazy
    (Map.of_alist_exn
       (module String)
       Act_common.Id.
         [ ("i686", of_string "x86.32")
         ; ("x86_64", of_string "x86.64")
         ; ("powerpc64le", of_string "ppc.64.le") ])

let emits_of_probe (probe_results : string) : Act_common.Id.t Or_error.t =
  Or_error.Let_syntax.(
    let%bind cpu, _, _ = to_target_triplet probe_results in
    Act_utils.My_map.find_or_error (Lazy.force cpu_map) cpu
      ~sexp_of_key:[%sexp_of: string] ~map_name:"GCC cpu map")
