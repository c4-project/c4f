(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

let run (input : Plumbing.Input.t) (output : Plumbing.Output.t) :
    unit Or_error.t =
  Or_error.(
    input |> Frontend.Fir.load >>| Act_fir.Litmus_stats.scrape
    >>= Act_utils.My_format.odump output
          (Fmt.vbox Act_fir.Litmus_stats.Statset.pp))
