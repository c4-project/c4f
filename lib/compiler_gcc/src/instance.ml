(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

(* These should generally encompass the current versions of gcc/clang that
   have minor versions supporting C11 atomics. *)

let gcc_range : int Sequence.t =
  Sequence.range ~start:`inclusive 4 ~stop:`inclusive 10

let clang_range : int Sequence.t =
  Sequence.range ~start:`inclusive 3 ~stop:`inclusive 10

let binary_names_for_type (ty : string) (ran : int Sequence.t) :
    string Sequence.t =
  Sequence.(append (singleton ty) (map ~f:(Printf.sprintf "%s-%d" ty) ran))

let binary_names : string Sequence.t =
  Sequence.append
    (binary_names_for_type "gcc" gcc_range)
    (binary_names_for_type "clang" clang_range)

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

(* Why do we do _both_ version _and_ dumpversion?

   dumpversion gives a fairly regular output across gcc, clang, and
   AppleClang, but doesn't mention the actual name of the compiler (which is
   useful for distinguishing between gcc and AppleClang). version does, but
   its version string is more free-form, and so it's difficult to use it for
   version info. *)
let probe_args = [|[|"--version"|]; [|"-dumpversion"|]; [|"-dumpmachine"|]|]

let to_target_cpu (s : string) : string Or_error.t =
  (* Target triplets are ambiguous and vary in length, so we don't try to
     parse anything after the CPU - yet.

     TODO(@MattWindsor91): consider whether we need to keep some of the
     target triplet around, for instance to resolve ABI issues. *)
  match String.lsplit2 (String.strip s) ~on:'-' with
  | Some (cpu, _) ->
      Ok cpu
  | None ->
      Or_error.errorf "malformed target triplet: %s" s

let cpu_map : Act_common.Id.t Map.M(String).t Lazy.t =
  lazy
    (Map.of_alist_exn
       (module String)
       Act_common.Id.
         [ ("i686", of_string "x86.32")
         ; ("x86_64", of_string "x86.64")
         ; ("arm", of_string "arm")
         ; ("powerpc64le", of_string "ppc.64.le") ])

let name_of_version (version : string) : string Or_error.t =
  match String.split_lines version with
  | vs :: _ -> (
    (* This is fairly heuristicy and hacky, but goes something like this:

       - AppleClang appears with a version starting with 'Apple clang'; so if
       we see 'Apple' at the start then we tack 'Clang' onto the end for
       clarity. - GCC seems to output argv[0], ie 'gcc-9' will appear as
       'gcc-9'; Clang doesn't. We'll just reduce both to 'gcc' and 'clang'
       respectively by assuming any suffix starting with '-' can be dropped. *)
    match String.split ~on:' ' vs with
    | "Apple" :: _ ->
        Ok "AppleClang"
    | n :: _ -> (
      match String.lsplit2 ~on:'-' n with
      | Some (n, _) ->
          Ok n
      | None ->
          Ok n )
    | _ ->
        Or_error.errorf "malformed version header: %s" vs )
  | _ ->
      Or_error.error_string "no output from --version"

let emits_of_dumpmachine (dumpmachine : string) : Act_common.Id.t Or_error.t
    =
  Or_error.Let_syntax.(
    let%bind cpu = to_target_cpu dumpmachine in
    Act_utils.My_map.find_or_error (Lazy.force cpu_map) cpu
      ~sexp_of_key:[%sexp_of: string] ~map_name:"GCC cpu map")

let info_of_probe_split ~(version : string) ~(dumpversion : string)
    ~(dumpmachine : string) : Act_compiler.Probe_info.t Or_error.t =
  Or_error.Let_syntax.(
    let%map name = name_of_version version
    and emits = emits_of_dumpmachine dumpmachine in
    {Act_compiler.Probe_info.name; emits; version= dumpversion})

let info_of_probe : string list -> Act_compiler.Probe_info.t Or_error.t =
  function
  | [version; dumpversion; dumpmachine] ->
      info_of_probe_split ~version ~dumpversion ~dumpmachine
  | probe_results ->
      Or_error.error_s
        [%message
          "unexpected probe result list" ~list:(probe_results : string list)]
