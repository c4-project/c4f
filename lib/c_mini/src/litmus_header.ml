(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Tx = Travesty_base_exts

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

module Change_set = struct
  type t = Constant.t Act_litmus.Header.Change_set.t

  let parse_pc (pc : string) :
      Constant.t Act_litmus.Postcondition.t Or_error.t =
    Or_error.(
      pc |> Act_c_lang.Frontend.Litmus_post.load_from_string
      >>= Convert.litmus_post)

  let try_map_keep_clear_replace (type a b)
      (x : [`Keep | `Clear | `Replace_with of a]) ~(f : a -> b Or_error.t) :
      [`Keep | `Clear | `Replace_with of b] Or_error.t =
    match x with
    | `Keep ->
        Or_error.return `Keep
    | `Clear ->
        Or_error.return `Clear
    | `Replace_with y ->
        Or_error.map ~f:(fun y' -> `Replace_with y') (f y)

  let of_args ?(name : [`Keep | `Replace_with of string] = `Keep)
      ?(postcondition : [`Keep | `Clear | `Replace_with of string] = `Keep)
      () : t Or_error.t =
    Or_error.Let_syntax.(
      let%map parsed_pc =
        try_map_keep_clear_replace ~f:parse_pc postcondition
      in
      Act_litmus.Header.Change_set.make ~name ~postcondition:parsed_pc ())
end

module Filters = struct
  (* replace-header and modify-header share similar boilerplate. *)
  let lift_on_header (ctx : 'aux_i Plumbing.Filter_context.t)
      (ic : Stdio.In_channel.t) (oc : Stdio.Out_channel.t)
      ~(f : 'aux_i -> t -> t) : unit Or_error.t =
    let aux = Plumbing.Filter_context.aux ctx in
    Or_error.Let_syntax.(
      let%bind test =
        Frontend.load_from_ic ic
          ~path:(Plumbing.Filter_context.input_path_string ctx)
      in
      let%map test' =
        Litmus.Test.try_map_header test ~f:(fun header ->
            Or_error.return (f aux header))
      in
      Litmus.Pp.print oc test')

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

  module Modify = Plumbing.Filter.Make (struct
    let name = "modify-header"

    type aux_i = Change_set.t

    type aux_o = unit

    let run =
      lift_on_header ~f:(fun changes header ->
          Act_litmus.Header.Change_set.apply changes ~header)
  end)

  module Replace = Plumbing.Filter.Make (struct
    let name = "replace-header"

    type aux_i = t

    type aux_o = unit

    let run = lift_on_header ~f:Fn.const
  end)
end
