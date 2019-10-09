(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

module Test_utils = struct
  let add_entries_exn ?(tag : Act_state.Observation.Entry_tag.t option)
      (entries : (string, string) List.Assoc.t list)
      (obs : Act_state.Observation.t) : Act_state.Observation.t =
    entries |> Entry.Test_utils.entries_exn
    |> (fun entries -> Act_state.Observation.add_many ?tag obs ~entries)
    |> Or_error.ok_exn
end
