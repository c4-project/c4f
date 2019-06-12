(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to permit
   persons to whom the Software is furnished to do so, subject to the
   following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
   NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
   DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
   OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
   USE OR OTHER DEALINGS IN THE SOFTWARE. *)

open Core_kernel
module Tx = Travesty_base_exts
module Ac = Act_common
module Pb = Plumbing
module Cc_target = Act_machine.Target

let not_tracking_symbols_warning : string =
  {|
    The set of known C variables is empty.

    This can lead to `act` making incorrect assumptions;
    for example, it may fail to work out which assembly symbols
    refer to heap locations.

    To fix this, specify `-c-globals 'symbol1,symbol2,etc'` (and/or `-c-locals`),
    or (if possible) use a C litmus test as input.
  |}

let warn_if_not_tracking_symbols (o : Ac.Output.t) :
    Ac.C_id.t list option -> unit = function
  | None ->
      Ac.Output.pw o "@[%a@]@." Fmt.paragraphs not_tracking_symbols_warning
  | Some _ ->
      ()

let asm_runner_of_target (tgt : Cc_target.t) :
    (module Act_asm.Runner_intf.Basic) Or_error.t =
  Language_support.asm_runner_from_arch (Cc_target.arch tgt)

let make_job_input (c_variables : Ac.C_variables.Map.t option)
    (config_fn : Ac.C_variables.Map.t option -> 'cfg)
    (passes : Set.M(Act_sanitiser.Pass_group).t) : 'cfg Act_asm.Job.t =
  let symbols =
    c_variables
    |> Option.map ~f:Ac.C_id.Map.keys
    |> Option.map ~f:(List.map ~f:Ac.C_id.to_string)
  in
  let config = config_fn c_variables in
  Act_asm.Job.make ~passes ~config ?symbols ()

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
