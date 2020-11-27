(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module Fir = Act_fir
end

module J :
  Plumbing.Jsonable_types.S with type t = Fir.Constant.t Act_litmus.Header.t =
Act_litmus.Header.Json (struct
  include Fir.Constant

  let parse_post_string (s : string) :
      Fir.Constant.t Act_litmus.Postcondition.t Or_error.t =
    Or_error.(
      s |> Frontend.Litmus_post.load_from_string
      >>= Act_litmus.Postcondition.With_errors.map_right_m
            ~f:Abstract_prim.constant)
end)

include J
include Plumbing.Loadable.Of_jsonable (J)
include Plumbing.Storable.Of_jsonable (J)

let equal = Act_litmus.Header.equal Fir.Constant.equal

module Change_set = struct
  type t = Fir.Constant.t Act_litmus.Header.Change_set.t

  let parse_pc (pc : string) :
      Fir.Constant.t Act_litmus.Postcondition.t Or_error.t =
    Or_error.(
      pc |> Frontend.Litmus_post.load_from_string >>= Abstract.litmus_post)

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
  let lift_on_header (input : Plumbing.Input.t) (output : Plumbing.Output.t)
      ~(f : t -> t) : unit Or_error.t =
    Or_error.(
      input |> Frontend.Fir.load
      >>= Fir.Litmus.Test.try_map_header ~f:(fun header -> Ok (f header))
      >>= Act_utils.My_format.odump output (Fmt.vbox Reify.pp_litmus))

  let run_dump (input : Plumbing.Input.t) (output : Plumbing.Output.t) :
      unit Or_error.t =
    Or_error.(
      input |> Frontend.Fir.load >>| Fir.Litmus.Test.header
      >>= store ~dest:output)

  let run_modify (input : Plumbing.Input.t) (output : Plumbing.Output.t)
      ~(changes : Change_set.t) : unit Or_error.t =
    lift_on_header input output ~f:(fun header ->
        Act_litmus.Header.Change_set.apply changes ~header)

  let run_replace (input : Plumbing.Input.t) (output : Plumbing.Output.t)
      ~(replacement : t) : unit Or_error.t =
    lift_on_header input output ~f:(Fn.const replacement)
end
