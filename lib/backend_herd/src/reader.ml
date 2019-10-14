(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Shc = Act_backend_herdtools_common

include Shc.Reader.Make (struct
  let try_parse_state_count (line : string) : int option =
    Option.try_with (fun () -> Caml.Scanf.sscanf line "States %d" Fn.id)

  let try_split_state_line (_ : Shc.Reader.Test_type.t) (line : string) :
      string Shc.Reader.State_line.t Or_error.t =
    (* Herd inputs don't have a histogram, so they don't have an occurrence
       count or witness accounting. *)
    Or_error.return (Shc.Reader.State_line.make line)
end)
