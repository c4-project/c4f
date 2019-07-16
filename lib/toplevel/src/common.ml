(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core_kernel
module Tx = Travesty_base_exts
module Ac = Act_common
module Pb = Plumbing
module Cc_target = Act_machine.Target

let asm_runner_of_target (tgt : Cc_target.t) :
    (module Act_asm.Runner_intf.Basic) Or_error.t =
  Language_support.asm_runner_from_arch (Cc_target.arch tgt)

let make_output_from_standard_args (args : Args.Standard.t) : Ac.Output.t =
  Ac.Output.make
    ~verbose:(Args.Standard.is_verbose args)
    ~warnings:(Args.Standard.are_warnings_enabled args)

let setup_colour (args : Args.Standard.t) : unit =
  let style_renderer = Args.Standard.colour args in
  Fmt_tty.setup_std_outputs ?style_renderer ()

let lift_command
    ?(compiler_predicate : Act_compiler.Property.t Blang.t option)
    ?(machine_predicate : Act_machine.Property.t Blang.t option)
    ?(sanitiser_passes : Act_sanitiser.Pass_group.Selector.t Blang.t option)
    ?(with_compiler_tests : bool option) (args : Args.Standard.t)
    ~(f : Ac.Output.t -> Act_config.Act.t -> unit Or_error.t) : unit =
  setup_colour args ;
  let o = make_output_from_standard_args args in
  let result =
    Or_error.(
      Args.Standard.config_file args
      |> Plumbing.Fpath_helpers.of_string
      >>= Language_support.load_and_process_config ?compiler_predicate
            ?machine_predicate ?sanitiser_passes ?with_compiler_tests
      >>= f o)
  in
  if Or_error.is_error result then (
    Ac.Output.print_error o result ;
    exit 1 )
