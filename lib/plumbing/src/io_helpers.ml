(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Stdio

let with_input_and_output (i : Input.t) (o : Output.t)
    ~(f : In_channel.t -> Out_channel.t -> 'a Or_error.t) : 'a Or_error.t =
  Input.with_input i ~f:(fun ic -> Output.with_output o ~f:(f ic))

let lift_to_raw_strings ~(f : 'i -> Input.t -> Output.t -> 'o Or_error.t)
    (aux_in : 'i) ~(infile : string option) ~(outfile : string option) :
    'o Or_error.t =
  Or_error.Let_syntax.(
    let%bind i = Input.of_string_opt infile
    and o = Output.of_string_opt outfile in
    f aux_in i o)

let lift_to_fpaths ~(f : 'i -> Input.t -> Output.t -> 'o Or_error.t)
    (aux_in : 'i) ~(infile : Fpath.t option) ~(outfile : Fpath.t option) :
    'o Or_error.t =
  let i = Input.of_fpath_opt infile and o = Output.of_fpath_opt outfile in
  f aux_in i o
