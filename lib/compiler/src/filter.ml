(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Ac = Act_common
module Pb = Plumbing

module type S =
  Pb.Filter_types.S with type aux_i = Mode.t and type aux_o = unit

module Make (S : Instance_types.S) : S = Pb.Filter.Make_files_only (struct
  type aux_i = Mode.t

  type aux_o = unit

  let name = "C compiler"

  let run (ctx : Mode.t Pb.Filter_context.t) ~(infile : Fpath.t) :
      outfile:Fpath.t -> unit Or_error.t =
    S.compile (Pb.Filter_context.aux ctx) ~infiles:[infile]
end)
