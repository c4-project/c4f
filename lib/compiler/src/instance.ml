(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Ac = Act_common
module Au = Act_utils
module Pb = Plumbing

module Make (B : Instance_types.Basic_with_run_info) : Instance_types.S =
struct
  include B

  let cmd = Spec.With_id.cmd B.cspec

  let make_argv_fun (spec : Spec.t) (mode : Mode.t) =
    let user_args = Spec.argv spec in
    let arch = Spec.emits spec in
    Pb.Runner.argv_one_file (fun ~input ~output ->
        Or_error.return
          (B.compile_args ~user_args ~arch ~mode ~infile:input
             ~outfile:output) )

  let compile_with_mode (mode : Mode.t) ~(infile : Fpath.t)
      ~(outfile : Fpath.t) =
    let spec = Spec.With_id.spec B.cspec in
    B.Runner.run_with_copy ~prog:cmd
      {input= Pb.Copy_spec.file infile; output= Pb.Copy_spec.file outfile}
      (make_argv_fun spec mode)

  let compile = compile_with_mode Mode.Assembly

  let test () = B.Runner.run ~prog:cmd B.test_args
end

module S_to_filter (S : Instance_types.S) :
  Pb.Filter_types.S with type aux_i = unit and type aux_o = unit =
Pb.Filter.Make_files_only (struct
  type aux_i = unit

  type aux_o = unit

  let name = "C compiler"

  let tmp_file_ext = Fn.const "s"

  let run _ = S.compile
end)

module Make_filter (B : Instance_types.Basic_with_run_info) :
  Pb.Filter_types.S with type aux_i = unit and type aux_o = unit =
  S_to_filter (Make (B))

module Chain_input = struct
  type next_mode = [`Preview | `No_compile | `Compile]

  type 'a t = {file_type: Ac.File_type.t; next: next_mode -> 'a}
  [@@deriving make, fields]
end

module Chain_with_compiler
    (Comp : Pb.Filter_types.S with type aux_i = unit and type aux_o = unit)
    (Onto : Pb.Filter_types.S) :
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

  let select ctx =
    let aux = Pb.Filter_context.aux ctx in
    let input = Pb.Filter_context.input ctx in
    let file_type = Chain_input.file_type aux in
    let next = Chain_input.next aux in
    let f = lift_next next in
    if Ac.File_type.is_c input file_type then `Both ((), f) else `One f
end)

module Fail (E : sig
  val error : Error.t
end) : Instance_types.S = struct
  let test () = Result.ok_unit

  let compile ~infile ~outfile =
    ignore infile ; ignore outfile ; Result.Error E.error
end
