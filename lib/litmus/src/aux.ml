(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Tx = Travesty_base_exts
module Ac = Act_common

type 'const t =
  { locations: Ac.C_id.t list option
  ; init: (Ac.C_id.t, 'const) List.Assoc.t [@default []]
  ; postcondition: 'const Ast_base.Postcondition.t option }
[@@deriving fields, make]

module Json (Const : sig
  type t

  include Pretty_printer.S with type t := t

  val to_json : t -> Yojson.Basic.t
end) =
struct
  let id_to_json (id : Ac.C_id.t) : Yojson.Basic.t =
    `String (Ac.C_id.to_string id)

  let opt (foo : 'a option) ~(f : 'a -> Yojson.Basic.t) : Yojson.Basic.t =
    Option.value_map foo ~f ~default:`Null

  let locations_to_json (xs : Ac.C_id.t list) : Yojson.Basic.t =
    `List (List.map ~f:id_to_json xs)

  let init_to_json (init : (Ac.C_id.t, Const.t) List.Assoc.t) :
      Yojson.Basic.t =
    `Assoc
      (Tx.Alist.bi_map init ~left:Ac.C_id.to_string ~right:Const.to_json)

  let postcondition_to_json (pc : Const.t Ast_base.Postcondition.t) :
      Yojson.Basic.t =
    `String
      (Fmt.strf "@[<h>%a@]" (Pp.Generic.pp_post ~pp_const:Const.pp) pc)

  let to_json (aux : Const.t t) : Yojson.Basic.t =
    `Assoc
      [ ("locations", opt ~f:locations_to_json (locations aux))
      ; ("init", init_to_json (init aux))
      ; ("postcondition", opt ~f:postcondition_to_json (postcondition aux))
      ]
end
