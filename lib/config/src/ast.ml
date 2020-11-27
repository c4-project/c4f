(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Act_common

module Pp_helpers = struct
  let pp_directive (pp_val : 'v Fmt.t) : (string * 'v) Fmt.t =
    Fmt.(hbox (pair ~sep:sp string pp_val))

  let pp_stanza_base (pp_header : 'h Fmt.t) (pp_directive : 'd Fmt.t) :
      ('h * 'd list) Fmt.t =
    Fmt.(
      vbox ~indent:2
        (pair ~sep:sp
           (hbox (pp_header ++ any "@ {"))
           (list ~sep:sp pp_directive))
      ++ any "@ }")

  let pp_simple_stanza (pp_directive : 'd Fmt.t) : (string * 'd list) Fmt.t =
    pp_stanza_base Fmt.string pp_directive

  let pp_bool : bool Fmt.t =
    Fmt.(using (fun b -> if b then "yes" else "no") string)

  let pp_id_directive : (string * Id.t) Fmt.t = pp_directive Id.pp
end

module Fuzz = struct
  module Flag_value = struct
    type t = Ratio of int * int | Exact of bool [@@deriving sexp]

    let pp (f : Formatter.t) : t -> unit = function
      | Ratio (d, n) ->
          Fmt.pf f "%d:%d" d n
      | Exact b ->
          Pp_helpers.pp_bool f b
  end

  module Setter = struct
    type t =
      | Param of Act_common.Id.t * int
      | Flag of Act_common.Id.t * Flag_value.t
    [@@deriving sexp]

    let pp (f : Formatter.t) : t -> unit = function
      | Param (id, v) ->
          Fmt.pf f "param@ %a@ to@ %d" Act_common.Id.pp id v
      | Flag (id, v) ->
          Fmt.pf f "flag@ %a@ to@ %a" Act_common.Id.pp id Flag_value.pp v
  end

  type t = Action of Id.t * int option | Set of Setter.t [@@deriving sexp]

  let pp (f : Formatter.t) : t -> unit =
    Pp_helpers.(
      function
      | Action (id, Some c) ->
          pp_directive Fmt.(pair ~sep:sp Id.pp int) f ("action", (id, c))
      | Action (id, None) ->
          pp_id_directive f ("action", id)
      | Set set ->
          pp_directive Setter.pp f ("set", set))
end

module Top = struct
  type t = Fuzz of Fuzz.t list [@sexp.list] [@@deriving sexp]

  let as_fuzz : t -> Fuzz.t list option = function Fuzz f -> Some f

  let pp (f : Formatter.t) : t -> unit =
    Pp_helpers.(
      function Fuzz fs -> pp_simple_stanza Fuzz.pp f ("fuzz", fs))
end

type t = (Top.t list[@sexp.list]) [@@deriving sexp]

let pp : t Fmt.t = Fmt.list ~sep:Fmt.(sp ++ cut) Top.pp
