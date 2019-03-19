(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation
   files (the "Software"), to deal in the Software without
   restriction, including without limitation the rights to use, copy,
   modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

open Core_kernel
open Utils

module Scope = struct
  module M = struct
    type t =
      | Unknown
      | Local
      | Global
    [@@deriving sexp, equal]
  end

  include M

  include Comparable.Make (struct
    (* The comparison scheme used here is very deliberate, hence
         why we write it out explicitly:

         - information about a scope > no information about a scope;
         - considering a variable as global > considering it as local *)

    include M

    let weight = function
      | Unknown -> 0
      | Local -> 1
      | Global -> 2
    ;;

    let compare = Travesty.T_fn.on weight Int.compare
  end)

  let is_global = function
    | Global -> true
    | Local | Unknown -> false
  ;;
end

module Initial_value = struct
  type t = int option [@@deriving sexp, compare, equal]
end

module Record = struct
  module M = struct
    type t =
      { scope : Scope.t
      ; initial_value : Initial_value.t
      }
    [@@deriving sexp, compare, equal, make, fields]
  end

  include M
  include Comparable.Make (M)

  let is_global (record : t) : bool = Scope.is_global (scope record)

  let resolve_clash : [`Left of t | `Right of t | `Both of t * t] -> t = function
    | `Left x | `Right x -> x
    | `Both (l, r) -> max l r
  ;;
end

module Map = struct
  type t = Record.t C_identifier.Map.t

  let of_single_scope_map (scope : Scope.t)
                          (cvars : Initial_value.t C_identifier.Map.t) : t =
    C_identifier.Map.map cvars ~f:(fun initial_value -> Record.make ~scope ~initial_value)
  ;;

  let of_single_scope_set (scope : Scope.t)
                          (cvars : C_identifier.Set.t) : t =
    let cvars_map = C_identifier.Set.to_map cvars ~f:(Fn.const None) in
    of_single_scope_map scope cvars_map
  ;;

  let resolve_cvar_clashes ~key value =
    ignore (key : C_identifier.t);
    Some (Record.resolve_clash value)
  ;;

  let of_value_maps_opt
      ?(locals : Initial_value.t C_identifier.Map.t option)
      ?(globals : Initial_value.t C_identifier.Map.t option)
      () : t option =
    let locals_map = Option.map ~f:(of_single_scope_map Local) locals in
    let globals_map = Option.map ~f:(of_single_scope_map Global) globals in
    Option.merge
      locals_map
      globals_map
      ~f:(C_identifier.Map.merge ~f:resolve_cvar_clashes)
  ;;
end
