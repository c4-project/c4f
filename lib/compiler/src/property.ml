(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core_kernel
module Ac = Act_common

type t = Id of Ac.Id.Property.t [@@deriving sexp, variants]

let tree_docs : Ac.Property.Tree_doc.t =
  [("id", {args= ["PROPERTY"]; details= {| See 'identifier predicates'. |}})]

let pp_tree : unit Fmt.t =
  Ac.Property.Tree_doc.pp tree_docs (List.map ~f:fst Variants.descriptions)

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
