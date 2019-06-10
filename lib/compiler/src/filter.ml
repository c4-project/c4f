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

  let tmp_file_ext (ctx : Mode.t Pb.Filter_context.t) : string =
    match Pb.Filter_context.aux ctx with Assembly -> "s" | Object -> "o"

  let run (ctx : Mode.t Pb.Filter_context.t) :
      infile:Fpath.t -> outfile:Fpath.t -> unit Or_error.t =
    S.compile (Pb.Filter_context.aux ctx)
end)

module Chain_input = struct
  type next_mode = [`Preview | `No_compile | `Compile]

  type 'a t =
    {mode: Mode.t; file_type: Ac.File_type.t; next: next_mode -> 'a}
  [@@deriving make, fields]
end

module Chain_with_compiler (Comp : S) (Onto : Pb.Filter_types.S) :
  Pb.Filter_types.S
  with type aux_i = Onto.aux_i Chain_input.t
   and type aux_o = Onto.aux_o =
Pb.Filter_chain.Make_conditional_first (struct
  module First = Comp
  module Second = Onto

  type aux_i = Onto.aux_i Chain_input.t

  type aux_o = Onto.aux_o

  let combine_output (_ : unit option) (o : Onto.aux_o) : aux_o = o

  let lift_next (next : Chain_input.next_mode -> 'a) :
      unit Pb.Chain_context.t -> 'a = function
    | Checking_ahead ->
        next `Preview
    | Skipped ->
        next `No_compile
    | Ran () ->
        next `Compile

  let select (ctx : Onto.aux_i Chain_input.t Pb.Filter_context.t) :
      [ `Both of Mode.t * (unit Pb.Chain_context.t -> Onto.aux_i)
      | `One of unit Pb.Chain_context.t -> Onto.aux_i ] =
    let aux = Pb.Filter_context.aux ctx in
    let m = Chain_input.mode aux in
    let input = Pb.Filter_context.input ctx in
    let file_type = Chain_input.file_type aux in
    let next = Chain_input.next aux in
    let f = lift_next next in
    if Ac.File_type.is_c input file_type then `Both (m, f) else `One f
end)
