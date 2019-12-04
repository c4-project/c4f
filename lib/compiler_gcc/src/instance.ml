(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

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

let test_args = ["--version"]
