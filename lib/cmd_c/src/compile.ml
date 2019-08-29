(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core_kernel
module Cq_spec = Act_machine.Qualified.Compiler

let run (args : Toplevel.Args.Standard.t Toplevel.Args.With_files.t)
    (_o : Act_common.Output.t) (global_cfg : Act_config.Global.t)
    ~(raw_target : Toplevel.Asm_target.t) ~(mode : Act_compiler.Mode.t) :
    unit Or_error.t =
  Or_error.Let_syntax.(
    let%bind cfg = Act_config.Act.of_global global_cfg in
    let%bind target = Toplevel.Asm_target.resolve ~cfg raw_target in
    let%bind (module R) =
      Toplevel.Language_support.Resolve_compiler_from_target
      .filter_from_spec target
    in
    let%bind input = Toplevel.Args.With_files.infile_source args in
    let%bind output = Toplevel.Args.With_files.outfile_sink args in
    Or_error.ignore_m (R.run mode input output))

let mode_type : Act_compiler.Mode.t Command.Arg_type.t =
  Command.Arg_type.of_alist_exn
    Act_compiler.Mode.[("assembly", Assembly); ("object", Object)]

let mode_param : Act_compiler.Mode.t Command.Param.t =
  Command.Param.flag_optional_with_default_doc "mode" mode_type
    ~default:Assembly
    ~doc:"MODE the output mode with which to invoke the compiler"
    Act_compiler.Mode.sexp_of_t

let readme () : string =
  Act_utils.My_string.format_for_readme
    {|
    This command runs a compiler, given its fully qualified identifier, on
    a single input file.  It either outputs an assembly (.s) file (the default),
    or an object (.o) file.
    |}

let command : Command.t =
  Command.basic ~summary:"run the given compiler on a single file" ~readme
    Command.Let_syntax.(
      let%map standard_args = Toplevel.Args.(With_files.get Standard.get)
      and raw_target = Toplevel.Args.asm_target
      and mode = mode_param in
      fun () ->
        Toplevel.Common.lift_command
          (Toplevel.Args.With_files.rest standard_args)
          ~f:(run standard_args ~raw_target ~mode))
