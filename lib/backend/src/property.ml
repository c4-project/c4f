(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core_kernel
module Ac = Act_common

type t = Id of Ac.Id.Property.t | Style of Ac.Id.Property.t
[@@deriving sexp, variants]

let names : string list Lazy.t = lazy (List.map ~f:fst Variants.descriptions)

let tree_docs : Ac.Property.Tree_doc.t =
  [ ("id", {args= ["PROPERTY"]; details= {| See 'identifier predicates'. |}})
  ; ( "style"
    , { args= ["PROPERTY"]
      ; details=
          {|
Selects backends whose 'style' (for example, 'herd' or 'litmus')
matches the given ID;
see 'identifier predicates'.
|}
      } ) ]

let pp_tree : unit Fmt.t =
  Ac.Property.Tree_doc.pp tree_docs (List.map ~f:fst Variants.descriptions)

let eval (cspec : Spec.With_id.t) = function
  | Id prop ->
      Ac.Id.Property.eval ~id:(Spec.With_id.id cspec) prop
  | Style prop ->
      Ac.Id.Property.eval ~id:(Spec.style (Spec.With_id.spec cspec)) prop

let eval_b expr cspec = Blang.eval expr (eval cspec)
