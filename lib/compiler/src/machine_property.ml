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
module Au = Act_utils
module Pb = Plumbing
open Act_common

type t = Id of Id.Property.t | Is_remote | Is_local
[@@deriving sexp, variants]

let eval (spec : Machine_spec.With_id.t) = function
  | Id prop ->
      Id.Property.eval (Machine_spec.With_id.id spec) prop
  | Is_remote ->
      Machine_spec.With_id.remoteness spec = `Remote
  | Is_local ->
      Machine_spec.With_id.remoteness spec = `Local

let eval_b spec expr = Blang.eval expr (eval spec)

let tree_docs : Property.Tree_doc.t =
  [ ("id", {args= ["PROPERTY"]; details= {| See 'identifier predicates'. |}})
  ; ( "is_remote"
    , { args= []
      ; details= {| Selects machines that are known to be remote. |} } )
  ; ( "is_local"
    , {args= []; details= {| Selects machines that are known to be local. |}}
    ) ]

let pp_tree : unit Fmt.t =
  Property.Tree_doc.pp tree_docs (List.map ~f:fst Variants.descriptions)

let%expect_test "all properties have documentation" =
  let num_passes =
    Variants.descriptions |> List.map ~f:fst
    |> List.map ~f:(List.Assoc.mem tree_docs ~equal:String.Caseless.equal)
    |> List.count ~f:not
  in
  Fmt.pr "@[<v>%d@]@." num_passes ;
  [%expect {| 0 |}]
