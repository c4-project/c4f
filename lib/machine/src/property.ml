(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core_kernel
module Ac = Act_common
module Au = Act_utils
module Pb = Plumbing

type t = Id of Ac.Id.Property.t | Is_remote | Is_local
[@@deriving sexp, variants]

let check_is_remote (spec : Spec.With_id.t) : bool =
  match Spec.With_id.remoteness spec with `Remote -> true | _ -> false

let check_is_local (spec : Spec.With_id.t) : bool =
  match Spec.With_id.remoteness spec with `Local -> true | _ -> false

let eval (spec : Spec.With_id.t) = function
  | Id prop ->
      Ac.Id.Property.eval ~id:(Spec.With_id.id spec) prop
  | Is_remote ->
      check_is_remote spec
  | Is_local ->
      check_is_local spec

let eval_b spec expr = Blang.eval expr (eval spec)

let names : string list Lazy.t = lazy (List.map ~f:fst Variants.descriptions)

let tree_docs : Ac.Property.Tree_doc.t =
  [ ("id", {args= ["PROPERTY"]; details= {| See 'identifier predicates'. |}})
  ; ( "is_remote"
    , {args= []; details= {| Selects machines that are known to be remote. |}}
    )
  ; ( "is_local"
    , {args= []; details= {| Selects machines that are known to be local. |}}
    ) ]

let property_names : string list = List.map ~f:fst Variants.descriptions

let pp_tree : unit Fmt.t = Ac.Property.Tree_doc.pp tree_docs property_names

let%expect_test "all properties have documentation" =
  let num_passes =
    property_names
    |> List.map ~f:(List.Assoc.mem tree_docs ~equal:String.Caseless.equal)
    |> List.count ~f:not
  in
  Fmt.pr "@[<v>%d@]@." num_passes ;
  [%expect {| 0 |}]
