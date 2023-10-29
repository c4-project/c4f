(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Ppx_yojson_conv_lib.Yojson_conv.Primitives
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

module Set = struct
  let yojson_of_set (type x c) (yojson_of_x : x -> Yojson.Safe.t)
      (set : (x, c) Set.t) : Yojson.Safe.t =
    set |> Set.to_list |> yojson_of_list yojson_of_x

  let set_of_yojson (type x c)
      (module C : Comparator.S
        with type t = x
         and type comparator_witness = c ) (x_of_yojson : Yojson.Safe.t -> x)
      (j : Yojson.Safe.t) : Set.M(C).t =
    j |> list_of_yojson x_of_yojson |> Set.of_list (module C)

  module Make (V : sig
    include Jsonable_types.S

    include Comparable.S with type t := t
  end) : Jsonable_types.S with type t = Set.M(V).t = struct
    type t = Set.M(V).t

    let yojson_of_t = yojson_of_set V.yojson_of_t

    let t_of_yojson = set_of_yojson (module V) V.t_of_yojson
  end
end

module Alist = struct
  let yojson_of_alist (type k v) (string_of_k : k -> string)
      (yojson_of_v : v -> Yojson.Safe.t) (l : (k, v) List.Assoc.t) :
      Yojson.Safe.t =
    `Assoc (Tx.Alist.bi_map ~left:string_of_k ~right:yojson_of_v l)

  let protect_key_of_string (type k) (k : string) ~(k_of_string : string -> k)
      : k Str_result.t =
    protect ~f:(fun () -> k_of_string k)

  let require_assoc (j : Yojson.Safe.t) :
      (string, Yojson.Safe.t) List.Assoc.t Str_result.t =
    protect ~f:(fun () -> Yojson.Safe.Util.to_assoc j)

  let alist_of_yojson' (type k v) (k_of_string : string -> k)
      (v_of_yojson' : Yojson.Safe.t -> v Str_result.t) :
      Yojson.Safe.t -> (k, v) List.Assoc.t Str_result.t =
    Sx.(
      require_assoc
      >=> AM.bi_map_m
            ~left:(protect_key_of_string ~k_of_string)
            ~right:v_of_yojson' )

  let alist_of_yojson (type k v) (k_of_string : string -> k)
      (v_of_yojson : Yojson.Safe.t -> v) (j : Yojson.Safe.t) :
      (k, v) List.Assoc.t =
    j |> Yojson.Safe.Util.to_assoc
    |> Tx.Alist.bi_map ~left:k_of_string ~right:v_of_yojson

  module Make (K : Stringable.S) (V : Jsonable_types.S) :
    Jsonable_types.S with type t = (K.t, V.t) List.Assoc.t = struct
    type t = (K.t, V.t) List.Assoc.t

    let yojson_of_t (x : t) : Yojson.Safe.t =
      `Assoc (Tx.Alist.bi_map ~left:K.to_string ~right:V.yojson_of_t x)

    let t_of_yojson : Yojson.Safe.t -> t =
      alist_of_yojson K.of_string V.t_of_yojson
  end
end

module Make_map (K : sig
  type t

  include Comparable.S with type t := t

  include Stringable.S with type t := t
end)
(V : Jsonable_types.S) : Jsonable_types.S with type t = V.t Map.M(K).t =
struct
  type t = V.t Map.M(K).t

  module Alist = Alist.Make (K) (V)

  let yojson_of_t : t -> Yojson.Safe.t =
    Fn.compose Alist.yojson_of_t Map.to_alist

  let t_of_yojson (j : Yojson.Safe.t) : t =
    j |> Alist.t_of_yojson |> Map.of_alist_exn (module K)
end

module Of_stringable (E : Stringable.S) :
  Jsonable_types.S with type t = E.t = struct
  type t = E.t

  let yojson_of_t (x : E.t) : Yojson.Safe.t = `String (E.to_string x)

  let t_of_yojson (j : Yojson.Safe.t) : E.t =
    j |> Yojson.Safe.Util.to_string |> E.of_string
end

module String : Jsonable_types.S with type t = string = Of_stringable (String)

module Option (B : Jsonable_types.S) :
  Jsonable_types.S with type t = B.t option = struct
  type t = B.t option [@@deriving yojson]
end
