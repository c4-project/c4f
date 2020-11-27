(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Filter_types

let wrap_error (name : string) : 'a Or_error.t -> 'a Or_error.t =
  Or_error.tag ~tag:(Printf.sprintf "In filter '%s'" name)

module Make (B : Basic) :
  S with type aux_i = B.aux_i and type aux_o = B.aux_o = struct
  include B

  let run (aux : aux_i) (input : Input.t) (output : Output.t) =
    let ctx = Filter_context.make ~aux ~input ~output in
    wrap_error name
      (Io_helpers.with_input_and_output ~f:(B.run ctx) input output)
end
