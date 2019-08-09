(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

module Make (B : Reader_types.Basic) : Reader_types.S = struct
  include B

  let read_output_from_string (s : string) : Output.t =
    Output.join (B.load_from_string s)
end
