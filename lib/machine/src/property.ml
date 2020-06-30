(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core_kernel

open struct
  module Ac = Act_common
  module Au = Act_utils
  module Pb = Plumbing
end

type t = Id of Ac.Id.Property.t | Is_remote | Is_local
[@@deriving sexp, variants]

let remoteness : Spec.With_id.t -> [`Local | `Remote | `Unknown] =
  Fn.compose Spec.remoteness Spec.With_id.spec

let check_is_remote (spec : Spec.With_id.t) : bool =
  match remoteness spec with `Remote -> true | _ -> false

let check_is_local (spec : Spec.With_id.t) : bool =
  match remoteness spec with `Local -> true | _ -> false

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

let pp_tree : unit Fmt.t =
  Ac.Property.Tree_doc.pp tree_docs (Lazy.force names)
