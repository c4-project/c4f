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

(* We can't easily derive yojson for the whole thing, since the
   postcondition serialisation depends on being able to pretty-print and
   parse the postcondition rather than recursively serialising it. *)

module Json (Const : sig
  type t

  include Pretty_printer.S with type t := t

  include Plumbing.Loadable_types.Jsonable with type t := t

  val parse_post_string : string -> t Ast_base.Postcondition.t Or_error.t
end) : Plumbing.Loadable_types.Jsonable with type t = Const.t t = struct
  type nonrec t = Const.t t

  let opt (foo : 'a option) ~(f : 'a -> Yojson.Safe.t) : Yojson.Safe.t =
    Option.value_map foo ~f ~default:`Null

  let postcondition_to_json (pc : Const.t Ast_base.Postcondition.t) :
      Yojson.Safe.t =
    `String
      (Fmt.strf "@[<h>%a@]" (Pp.Generic.pp_post ~pp_const:Const.pp) pc)

  let to_yojson (aux : t) : Yojson.Safe.t =
    (* Need to open Caml because some of to_yojson's code depends on the
       stdlib versions of some things Base shadows. *)
    `Assoc
      Caml.
        [ ("locations", [%to_yojson: Ac.C_id.t list option] (locations aux))
        ; ("init", [%to_yojson: Const.t Ac.C_id.Alist.t] (init aux))
        ; ("postcondition", opt ~f:postcondition_to_json (postcondition aux))
        ]

  module U = Yojson.Safe.Util

  let postcondition_of_json (json : Yojson.Safe.t) : (Const.t Ast_base.Postcondition.t option, string) Result.t =
    let result =
      Or_error.Let_syntax.(
        let%bind post_str = Or_error.try_with (fun () -> U.to_string_option json) in
        Tx.Option.With_errors.map_m ~f:Const.parse_post_string post_str
      )
    in Result.map_error ~f:Error.to_string_hum result

  let of_yojson (json : Yojson.Safe.t) : (t, string) Result.t =
    Result.Let_syntax.(
      let %bind locations = [%of_yojson: Ac.C_id.t list option] (U.member "locations" json) in
      let %bind init = [%of_yojson: Const.t Ac.C_id.Alist.t] (U.member "init" json) in
      let %map postcondition = postcondition_of_json (U.member "postcondition" json) in
      make ?locations ~init ?postcondition ()
    )

  let of_yojson_exn (json : Yojson.Safe.t) : t =
    Result.ok_or_failwith (of_yojson json)
end
