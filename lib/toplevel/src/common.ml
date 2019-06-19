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

(** Used as input to {{!Make_lifter} Make_lifter}. *)
module type Basic_lifter = sig
  type t

  val as_standard_args : t -> Args.Standard.t
end

let setup_colour (args : Args.Standard.t) : unit =
  let style_renderer = Args.Standard.colour args in
  Fmt_tty.setup_std_outputs ?style_renderer ()

(** Since the command lifters are fairly uniform except for the specific
    argument bundle they pass through, we use a functor to construct them. *)
module Make_lifter (B : Basic_lifter) = struct
  let lift ?compiler_predicate ?machine_predicate ?sanitiser_passes
      ?with_compiler_tests
      ~(f : B.t -> Ac.Output.t -> Act_config.Act.t -> unit Or_error.t)
      (args : B.t) : unit =
    let standard_args = B.as_standard_args args in
    setup_colour standard_args ;
    let o = make_output_from_standard_args standard_args in
    Or_error.(
      Args.Standard.config_file standard_args
      |> Plumbing.Fpath_helpers.of_string
      >>= Language_support.load_and_process_config ?compiler_predicate
            ?machine_predicate ?sanitiser_passes ?with_compiler_tests
      >>= f args o)
    |> Ac.Output.print_error o
end

module Standard_lifter = Make_lifter (Args.Standard)

let lift_command = Standard_lifter.lift

module With_files_lifter = Make_lifter (Args.Standard_with_files)

let lift_command_with_files = With_files_lifter.lift

module Asm_lifter = Make_lifter (Args.Standard_asm)

let lift_asm_command_basic
    ~(f :
          Args.Standard_asm.t
       -> Ac.Output.t
       -> Act_config.Act.t
       -> unit Or_error.t) (args : Args.Standard_asm.t) : unit =
  Asm_lifter.lift
    ?sanitiser_passes:(Args.Standard_asm.sanitiser_passes args)
    ~f args
