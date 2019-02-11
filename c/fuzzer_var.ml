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

module Value = struct
  type t =
    | Known_int of int
end

module Record = struct
  type t =
    { ty : Mini.Type.t option
    ; source : [ `Existing | `Generated ]
    ; scope : [ `Global | `Local ]
    ; value : Value.t option
    }
  [@@deriving fields]
  ;;

  let is_global : t -> bool = function
    | { scope=`Global ; _ } -> true
    | { scope=`Local  ; _ } -> false
  ;;

  let is_atomic : t -> bool = function
    | { ty = Some ty ; _ } -> Mini.Type.is_atomic ty
    | { ty = None    ; _ } -> false
  ;;

  let was_generated : t -> bool = function
    | { source = `Generated ; _ } -> true
    | { source = `Existing  ; _ } -> false
  ;;

  let erase_value (record : t) : t =
    { record with value = None }
  ;;

  let make_existing_global (ty : Mini.Type.t) : t =
    { ty     = Some ty
    ; source = `Existing
    ; scope  = `Global
    ; value  = None
    }
  ;;

  let make_existing_local (_name : C_identifier.t) : t =
    { ty     = None
    ; source = `Existing
    ; scope  = `Local
    ; value  = None
    }
  ;;

  let make_generated_global
    ?(initial_value : Value.t option)
    (ty             : Mini.Type.t)
    : t =
    { ty     = Some ty
    ; source = `Generated
    ; scope  = `Global
    ; value  = initial_value
    }
  ;;
end

module Map = struct
  type t = Record.t C_identifier.Map.t

  let make_existing_var_map
    (globals : Mini.Type.t C_identifier.Map.t)
    (locals  : C_identifier.Set.t)
    : t =
    let globals_map = C_identifier.Map.map globals
        ~f:Record.make_existing_global
    in
    let locals_map = C_identifier.Set.to_map locals
        ~f:Record.make_existing_local
    in
    C_identifier.Map.merge globals_map locals_map
      ~f:(fun ~key -> ignore key;
           function
           | `Left x | `Right x | `Both (x, _) -> Some x
         )

  let register_global
      ?(initial_value : Value.t option)
      (map : t)
      (key : C_identifier.t)
      (ty : Mini.Type.t)
    : t =
    let data = Record.make_generated_global ?initial_value ty in
    C_identifier.Map.set map ~key ~data
  ;;

  let erase_value (map : t) ~(var : C_identifier.t) : t =
    C_identifier.Map.change map var
      ~f:(Option.map ~f:Record.erase_value)
  ;;
end
