(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

type t = Constant.t Act_litmus.Header.t [@@deriving equal]

module J : Plumbing.Jsonable_types.S with type t := t =
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
