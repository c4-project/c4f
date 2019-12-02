(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

let mode_args : Act_compiler.Mode.t -> string list = function
  | Assembly ->
      ["-S"]
  | Object ->
      ["-c"]
  | Binary ->
      []

let no_position_independence_args : string list = ["-fno-pic"]

let output_args (file : string) : string list = ["-o"; file]

let compile_args ~user_args ~(arch : Act_common.Id.t)
    ~(mode : Act_compiler.Mode.t) ~(infiles : string list)
    ~(outfile : string) =
  ignore arch ;
  List.concat
    [ mode_args mode
    ; no_position_independence_args
    ; user_args
    ; output_args outfile
    ; infiles ]

let test_args = ["--version"]
