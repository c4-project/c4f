(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to permit
   persons to whom the Software is furnished to do so, subject to the
   following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
   NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
   DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
   OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
   USE OR OTHER DEALINGS IN THE SOFTWARE. *)

open Core_kernel
module Ac = Act_common
module Au = Act_utils
module Pb = Plumbing
open Instance_types

module Property = struct
  type t = Id of Ac.Id.Property.t [@@deriving sexp, variants]

  let tree_docs : Ac.Property.Tree_doc.t =
    [ ( "id"
      , {args= ["PROPERTY"]; details= {| See 'identifier predicates'. |}} )
    ]

  let pp_tree : unit Fmt.t =
    Ac.Property.Tree_doc.pp tree_docs
      (List.map ~f:fst Variants.descriptions)

  let%expect_test "all properties have documentation" =
    let num_passes =
      Variants.descriptions |> List.map ~f:fst
      |> List.map ~f:(List.Assoc.mem tree_docs ~equal:String.Caseless.equal)
      |> List.count ~f:not
    in
    Fmt.pr "@[<v>%d@]@." num_passes ;
    [%expect {| 0 |}]

  let eval (cspec : Spec.With_id.t) = function
    | Id prop ->
        Ac.Id.Property.eval (Spec.With_id.id cspec) prop

  let eval_b cspec expr = Blang.eval expr (eval cspec)
end

module type Basic_with_run_info = sig
  include Basic

  val cspec : Spec.With_id.t

  module Runner : Pb.Runner_types.S
end

module Make (B : Basic_with_run_info) : S = struct
  include B

  let cmd = Spec.With_id.cmd B.cspec

  let compile ~(infile : Fpath.t) ~(outfile : Fpath.t) =
    let s = Spec.With_id.spec B.cspec in
    let argv_fun =
      Pb.Runner.argv_one_file (fun ~input ~output ->
          Or_error.return
            (B.compile_args ~args:(Spec.argv s) ~emits:(Spec.emits s)
               ~infile:input ~outfile:output) )
    in
    B.Runner.run_with_copy ~prog:cmd
      {input= Pb.Copy_spec.file infile; output= Pb.Copy_spec.file outfile}
      argv_fun

  let test () = B.Runner.run ~prog:cmd B.test_args
end

module S_to_filter (S : S) :
  Pb.Filter_types.S with type aux_i = unit and type aux_o = unit =
Pb.Filter.Make_files_only (struct
  type aux_i = unit

  type aux_o = unit

  let name = "C compiler"

  let tmp_file_ext = Fn.const "s"

  let run _ = S.compile
end)

module Make_filter (B : Basic_with_run_info) :
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
end) : S = struct
  let test () = Result.ok_unit

  let compile ~infile ~outfile =
    ignore infile ; ignore outfile ; Result.Error E.error
end
