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
    ;;
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
        | Local   -> 1
        | Global  -> 2
      ;;
      let compare = Travesty.T_fn.on weight Int.compare
    end)

  let brand
      (scope : t)
      (cvars : C_identifier.Set.t) : t C_identifier.Map.t =
    C_identifier.Set.to_map cvars ~f:(Fn.const scope)
  ;;

  let resolve_cvar_clashes ~key =
    ignore key;
    function
    | `Left ty | `Right ty -> Some ty
    | `Both (l, r) -> Some (max l r)
  ;;

  let make_map_opt
      ?(locals : C_identifier.Set.t option)
      ?(globals : C_identifier.Set.t option)
      ()
    : t C_identifier.Map.t option =
    let locals_map  = Option.map ~f:(brand Local ) locals in
    let globals_map = Option.map ~f:(brand Global) globals in
    Option.merge locals_map globals_map
      ~f:(C_identifier.Map.merge ~f:resolve_cvar_clashes)
  ;;
end

module Map = struct
  type t = Scope.t C_identifier.Map.t
end
