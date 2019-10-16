(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

module J :
  Plumbing.Jsonable_types.S with type t = Constant.t Act_litmus.Header.t =
Act_litmus.Header.Json (struct
  include Constant

  let parse_post_string (s : string) :
      Constant.t Act_litmus.Postcondition.t Or_error.t =
    Or_error.(
      s |> Act_c_lang.Frontend.Litmus_post.load_from_string
      >>= Act_litmus.Postcondition.With_errors.map_right_m
            ~f:Convert.constant)
end)

include J
include Plumbing.Loadable.Of_jsonable (J)
include Plumbing.Storable.Of_jsonable (J)

let equal = Act_litmus.Header.equal Constant.equal

module Filters = struct
  module Dump = Plumbing.Filter.Make (struct
    let name = "dump-header"

    type aux_i = unit

    type aux_o = unit

    let run (ctx : aux_i Plumbing.Filter_context.t)
        (ic : Stdio.In_channel.t) (oc : Stdio.Out_channel.t) :
        aux_o Or_error.t =
      Or_error.Let_syntax.(
        let%bind test =
          Frontend.load_from_ic ic
            ~path:(Plumbing.Filter_context.input_path_string ctx)
        in
        store_to_oc ~dest:oc (Litmus.Test.header test))
  end)

  module Replace = Plumbing.Filter.Make (struct
    let name = "replace-header"

    type aux_i = t

    type aux_o = unit

    let run (ctx : aux_i Plumbing.Filter_context.t)
        (ic : Stdio.In_channel.t) (oc : Stdio.Out_channel.t) :
        aux_o Or_error.t =
      let header = Plumbing.Filter_context.aux ctx in
      Or_error.Let_syntax.(
        let%bind test =
          Frontend.load_from_ic ic
            ~path:(Plumbing.Filter_context.input_path_string ctx)
        in
        let%map test' =
          Litmus.Test.try_map_header test ~f:(fun _ ->
              Or_error.return header)
        in
        Litmus.Pp.print oc test')
  end)
end
