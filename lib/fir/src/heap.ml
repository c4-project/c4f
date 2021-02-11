(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

module Cache = struct
  type t = Constant.t Map.M(Address).t

  let empty () : t = Map.empty (module Address)

  let load (c : t) ~(address : Address.t) : Constant.t option =
    Map.find c (Address.normalise address)

  let store (c : t) ~(address : Address.t) ~(data : Constant.t) : t =
    Map.set c ~key:(Address.normalise address) ~data
end

type t = {cache: Cache.t; source: Address.t -> Constant.t Or_error.t}

let empty_source : Address.t -> Constant.t Or_error.t =
  Fn.const
    (Or_error.error_string
       "tried to access an address in an empty environment" )

let make (source : Address.t -> Constant.t Or_error.t) : t =
  {cache= Cache.empty (); source}

let empty () : t = make empty_source

let load (heap : t) ~(address : Address.t) : Constant.t Or_error.t =
  match Cache.load heap.cache ~address with
  | Some k -> Ok k
  | None -> heap.source address

let store (heap : t) ~(address : Address.t) ~(data : Constant.t) : t =
  {heap with cache= Cache.store heap.cache ~address ~data}

module Monad = struct
  module Main :
    Travesty.State_transform_types.S
      with type state := t
       and type 'a Inner.t = 'a Or_error.t =
  Travesty.State_transform.Make (struct
    type nonrec t = t

    module Inner = Or_error
  end)

  include Main

  let load (address : Address.t) : Constant.t t =
    Main.Monadic.peek (load ~address)

  let store (address : Address.t) (data : Constant.t) : unit t =
    Main.modify (store ~address ~data)
end
