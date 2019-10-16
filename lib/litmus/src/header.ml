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
  ; init: (Ac.C_id.t * 'const) list
  ; postcondition: 'const Postcondition.t option
  ; name: string }
[@@deriving fields, make, equal, sexp]

let empty (type k) : k t =
  {name= ""; locations= None; init= []; postcondition= None}

let add_global (type k) (aux : k t) ~(name : Ac.C_id.t) ~(initial_value : k)
    : k t Or_error.t =
  (* TODO(@MattWindsor91): check for duplicates. *)
  Or_error.return
    { name= aux.name
    ; locations= Option.map ~f:(fun ls -> name :: ls) aux.locations
    ; init= (name, initial_value) :: aux.init
    ; postcondition= aux.postcondition }

let map_name (type k) (header : k t) ~(f : string -> string) : k t =
  {header with name= f header.name}

let map_tids (type k) (aux : k t) ~(f : int -> int) : k t =
  let post = postcondition aux in
  let post' =
    Option.map ~f:(Postcondition.map_left ~f:(Ac.Litmus_id.map_tid ~f)) post
  in
  {aux with postcondition= post'}

module BT :
  Travesty.Bi_traversable_types.S1_right
    with type 'const t := 'const t
     and type left = Act_common.C_id.t =
Travesty.Bi_traversable.Make1_right (struct
  type nonrec 'const t = 'const t

  type left = Act_common.C_id.t

  module On_monad (M : Monad.S) = struct
    module MA = Travesty_base_exts.Alist.On_monad (M)
    module PostO =
      Travesty.Bi_traversable.Chain_Bi1_right_Traverse1
        (Postcondition.On_c_identifiers)
        (Travesty_base_exts.Option)
    module MP = PostO.On_monad (M)
    module Mx = Travesty.Monad_exts.Extend (M)

    let bi_map_m (type a b) (aux : a t) ~(left : Ac.C_id.t -> Ac.C_id.t M.t)
        ~(right : a -> b M.t) : b t M.t =
      let name = name aux in
      let locations = locations aux in
      M.Let_syntax.(
        let%map init = MA.bi_map_m (init aux) ~left ~right
        and postcondition = MP.bi_map_m (postcondition aux) ~left ~right in
        make ~name ~init ?locations ?postcondition ())
  end
end)

include BT

(* We can't easily derive yojson for the whole thing, since the
   postcondition serialisation depends on being able to pretty-print and
   parse the postcondition rather than recursively serialising it. *)

module Json (Const : sig
  type t

  include Pretty_printer.S with type t := t

  include Plumbing.Jsonable_types.S with type t := t

  val parse_post_string : string -> t Postcondition.t Or_error.t
end) : Plumbing.Jsonable_types.S with type t = Const.t t = struct
  type nonrec t = Const.t t

  let opt (foo : 'a option) ~(f : 'a -> Yojson.Safe.t) : Yojson.Safe.t =
    Option.value_map foo ~f ~default:`Null

  let postcondition_to_json (pc : Const.t Postcondition.t) : Yojson.Safe.t =
    `String (Fmt.strf "@[<h>%a@]" (Postcondition.pp ~pp_const:Const.pp) pc)

  let yojson_of_t (aux : t) : Yojson.Safe.t =
    `Assoc
      [ ("name", [%yojson_of: string] (name aux))
      ; ("locations", [%yojson_of: Ac.C_id.t list option] (locations aux))
      ; ("init", [%yojson_of: Const.t Ac.C_id.Alist.t] (init aux))
      ; ("postcondition", opt ~f:postcondition_to_json (postcondition aux))
      ]

  module U = Yojson.Safe.Util

  let postcondition_of_json (json : Yojson.Safe.t) :
      (Const.t Postcondition.t option, string) Result.t =
    let result =
      Or_error.Let_syntax.(
        let%bind post_str =
          Or_error.try_with (fun () -> U.to_string_option json)
        in
        Tx.Option.With_errors.map_m ~f:Const.parse_post_string post_str)
    in
    Result.map_error ~f:Error.to_string_hum result

  let t_of_yojson (json : Yojson.Safe.t) : t =
    let name = [%of_yojson: string] (U.member "name" json) in
    let locations =
      [%of_yojson: Ac.C_id.t list option] (U.member "locations" json)
    in
    let init =
      [%of_yojson: Const.t Ac.C_id.Alist.t] (U.member "init" json)
    in
    let postcondition =
      Result.ok_or_failwith
        (postcondition_of_json (U.member "postcondition" json))
    in
    make ~name ?locations ~init ?postcondition ()

  let t_of_yojson' (json : Yojson.Safe.t) : (t, string) Result.t =
    Result.try_with (fun () -> t_of_yojson json)
    |> Result.map_error ~f:Exn.to_string
end

module Change_set = struct
  module Hdr_fields = Fields

  type 'const hdr = 'const t

  type 'const t =
    { name: [`Keep | `Replace_with of string] [@default `Keep]
    ; postcondition:
        [`Keep | `Clear | `Replace_with of 'const Postcondition.t]
          [@default `Keep] }
  [@@deriving fields, make]

  let apply_keep_replace (type a const)
      (field : ([> `Set_and_create], const hdr, a) Field.t_with_perm)
      (change : [< `Keep | `Replace_with of a]) (header : const hdr) :
      const hdr =
    match change with
    | `Keep ->
        header
    | `Replace_with x ->
        Field.fset field header x

  let apply_keep_clear_replace (type a const)
      (field : ([> `Set_and_create], const hdr, a option) Field.t_with_perm)
      (change : [`Keep | `Clear | `Replace_with of a]) (header : const hdr)
      : const hdr =
    match change with
    | `Clear ->
        apply_keep_replace field (`Replace_with None) header
    | `Keep ->
        header
    | `Replace_with x ->
        apply_keep_replace field (`Replace_with (Some x)) header

  let apply_postcondition
      (change : [`Keep | `Clear | `Replace_with of 'const Postcondition.t])
      (header : 'const hdr) : 'const hdr =
    apply_keep_clear_replace Hdr_fields.postcondition change header

  let apply_name (change : [`Keep | `Replace_with of string])
      (header : 'const hdr) : 'const hdr =
    apply_keep_replace Hdr_fields.name change header

  let apply (change : 'const t) ~(header : 'const hdr) : 'const hdr =
    header
    |> apply_postcondition change.postcondition
    |> apply_name change.name
end
