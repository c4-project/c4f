(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Tx = Travesty_base_exts

module Str_result : Monad.S with type 'a t = ('a, string) Result.t = struct
  type 'a t = ('a, string) Result.t

  include (Result : Monad.S with type 'a t := ('a, string) Result.t)
end

module Sx = Travesty.Monad_exts.Extend (Str_result)
module AM = Tx.Alist.On_monad (Str_result)
module OM = Tx.Option.On_monad (Str_result)

let protect ~(f : unit -> 'a) : 'a Str_result.t =
  let r = Result.try_with f in
  Result.map_error r ~f:Exn.to_string

module Make_alist (K : Stringable.S) (V : Jsonable_types.S) :
  Jsonable_types.S with type t = (K.t, V.t) List.Assoc.t = struct
  type t = (K.t, V.t) List.Assoc.t

  let to_yojson (x : t) : Yojson.Safe.t =
    `Assoc (Tx.Alist.bi_map ~left:K.to_string ~right:V.to_yojson x)

  let protect_key_of_string (k : string) : K.t Str_result.t =
    protect ~f:(fun () -> K.of_string k)

  let require_assoc (j : Yojson.Safe.t) :
      (string, Yojson.Safe.t) List.Assoc.t Str_result.t =
    protect ~f:(fun () -> Yojson.Safe.Util.to_assoc j)

  let of_yojson : Yojson.Safe.t -> t Str_result.t =
    Sx.(
      require_assoc
      >=> AM.bi_map_m ~left:protect_key_of_string ~right:V.of_yojson)
end

module Make_map (K : sig
  type t

  include Comparable.S with type t := t

  include Stringable.S with type t := t
end)
(V : Jsonable_types.S) : Jsonable_types.S with type t = V.t Map.M(K).t =
struct
  type t = V.t Map.M(K).t

  module Alist = Make_alist (K) (V)

  let to_yojson : t -> Yojson.Safe.t =
    Fn.compose Alist.to_yojson Map.to_alist

  let of_alist : (K.t, V.t) List.Assoc.t -> t Str_result.t =
    Fn.compose
      (Result.map_error ~f:Error.to_string_hum)
      (Map.of_alist_or_error (module K))

  let of_yojson : Yojson.Safe.t -> t Str_result.t =
    Sx.(Alist.of_yojson >=> of_alist)
end

module String : Jsonable_types.S with type t = string = struct
  type t = string

  let to_yojson (s : string) : Yojson.Safe.t = `String s

  let of_yojson (j : Yojson.Safe.t) : string Str_result.t =
    protect ~f:(fun () -> Yojson.Safe.Util.to_string j)
end

module Option (B : Jsonable_types.S) :
  Jsonable_types.S with type t = B.t option = struct
  type t = B.t option

  let to_yojson : B.t option -> Yojson.Safe.t =
    Option.value_map ~f:B.to_yojson ~default:`Null

  let of_yojson (j : Yojson.Safe.t) : B.t option Str_result.t =
    let err_opt = Yojson.Safe.Util.to_option B.of_yojson j in
    OM.sequence_m err_opt
end
