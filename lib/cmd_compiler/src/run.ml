(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core_kernel

open struct
  module Cq_spec = Act_machine.Qualified.Compiler
end

(* This used to use Act_compiler.Filter.Make directly, but filters don't
   support >1 file. However, we _do_ want to be able to make use of a lot of
   the filter infrastructure for dealing with stdin/stdout, so we compromise:
   if more than one file is given as input, we fall back to not using a
   filter.

   TODO(@MattWindsor91): maybe generalise filters so that we don't have this
   issue. *)

let resolve_compiler_instance (target : Cq_spec.t Act_machine.Target.t)
    ~(user_args : string list) :
    (module Act_compiler.Instance_types.S) Or_error.t =
  Or_error.(
    target |> Act_machine.Target.ensure_spec
    >>| Act_machine.Qualified.On_specs.map_left
          ~f:(Act_compiler.Spec.append_argv ~more_argv:user_args)
    >>= Common_cmd.Language_support.resolve)

let run_single (module Compiler : Act_compiler.Instance_types.S)
    (mode : Act_compiler.Mode.t) (input : Plumbing.Input.t)
    (output : Plumbing.Output.t) : unit Or_error.t =
  let module Filter = Act_compiler.Filter.Make (Compiler) in
  Filter.run mode input output

let run_multiple (module Compiler : Act_compiler.Instance_types.S)
    (mode : Act_compiler.Mode.t) (infiles : Fpath.t list)
    (output : Plumbing.Output.t) : unit Or_error.t =
  Or_error.Let_syntax.(
    let%bind outfile =
      Or_error.tag
        (Plumbing.Output.to_fpath_err output)
        ~tag:
          "Multi-input compiles require an explicit output file; this is a \
           known issue"
    in
    Compiler.compile mode ~infiles ~outfile)

let flag_to_arg : string -> string =
  Act_utils.My_string.ensure_prefix ~prefix:"-"

let run (args : Common_cmd.Args.Standard.t Common_cmd.Args.With_files.t)
    (_o : Act_common.Output.t) (cfg : Act_config.Global.t)
    ~(raw_target : Common_cmd.Asm_target.t) ~(mode : Act_compiler.Mode.t)
    ~(user_flags : string list) : unit Or_error.t =
  let user_args = List.map ~f:flag_to_arg user_flags in
  Or_error.Let_syntax.(
    let%bind target = Common_cmd.Asm_target.resolve ~cfg raw_target in
    let%bind (module Compiler) =
      resolve_compiler_instance ~user_args target
    in
    let%bind output = Common_cmd.Args.With_files.outfile_sink args in
    match%bind Common_cmd.Args.With_files.infiles_fpath args with
    | [] ->
        run_single
          (module Compiler)
          mode
          (Plumbing.Input.stdin ~file_type:"c" ())
          output
    | [input] ->
        run_single
          (module Compiler)
          mode
          (Plumbing.Input.of_fpath input)
          output
    | inputs ->
        run_multiple (module Compiler) mode inputs output)

let mode_alist : (string, Act_compiler.Mode.t) List.Assoc.t =
  List.Assoc.inverse Act_compiler.Mode.table

let mode_type : Act_compiler.Mode.t Command.Arg_type.t =
  Command.Arg_type.of_alist_exn mode_alist

let mode_param : Act_compiler.Mode.t Command.Param.t =
  Command.Param.flag_optional_with_default_doc "mode" mode_type
    ~default:Assembly
    ~doc:"MODE the output mode with which to invoke the compiler"
    Act_compiler.Mode.sexp_of_t

let readme () : string =
  Act_utils.My_string.format_for_readme
    {|
    This command runs a compiler, given its fully qualified identifier, on
    one or more input file.  It either outputs an assembly (.s) file
    (the default), or an object (.o) file.

    When taking more than one file as input, an output file must be given.
    |}

let command : Command.t =
  Command.basic ~summary:"run the given compiler on a single file" ~readme
    Command.Let_syntax.(
      let%map_open standard_args =
        Common_cmd.Args.(With_files.get_with_multiple_inputs Standard.get)
      and raw_target = Common_cmd.Args.asm_target
      and mode = mode_param
      and user_flags =
        flag "-arg"
          ~doc:
            "STRING an extra flag to supply to the compiler, less its \
             initial '-'"
          (listed string)
      in
      fun () ->
        Common_cmd.Common.lift_command
          (Common_cmd.Args.With_files.rest standard_args)
          ~f:(run standard_args ~raw_target ~mode ~user_flags))
