(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Tx = Travesty_base_exts

module Make (R : Runner_intf.Runnable) :
  Runner_intf.S with type cfg = R.cfg = struct
  type cfg = R.cfg

  let parse isrc =
    let iname = Plumbing.Input.to_string isrc in
    Or_error.tag_arg
      (R.Program.load isrc)
      "Error while parsing assembly" iname String.sexp_of_t

  let in_source_to_basename (is : Plumbing.Input.t) : string =
    is |> Plumbing.Input.to_fpath_opt
    |> Option.value_map
         ~f:(fun fn -> Fpath.(fn |> rem_ext |> basename))
         ~default:"stdin"

  include Plumbing.Filter.Make (struct
    type aux_i = R.cfg Job.t

    type aux_o = Job.Output.t

    let name = R.name

    let run (ctx : R.cfg Job.t Plumbing.Filter_context.t) _ oc :
        Job.Output.t Or_error.t =
      let aux = Plumbing.Filter_context.aux ctx in
      let input = Plumbing.Filter_context.input ctx in
      let in_name = in_source_to_basename input in
      Or_error.Let_syntax.(
        let%bind program = parse input in
        let config =
          Tx.Option.value_f (Job.config aux) ~default_f:R.default_config
        in
        let passes = Job.passes aux in
        R.run ~in_name ~program ~config ~passes oc)
  end)
end
