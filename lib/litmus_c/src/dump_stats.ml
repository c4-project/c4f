(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

let run (input : Plumbing.Input.t) (output : Plumbing.Output.t) :
    unit Or_error.t =
  Or_error.(
    input |> Frontend.Fir.load >>| C4f_fir.Litmus_stats.scrape
    >>= C4f_utils.My_format.odump output
          (Fmt.vbox C4f_fir.Litmus_stats.Statset.pp))
