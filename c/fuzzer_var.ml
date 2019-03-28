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
  type t = Int of int
end

module Known_value = struct
  type t =
    { value : Value.t
    ; has_dependencies : bool
    }
  [@@deriving fields, make]

  let add_dependency (kv : t) : t = { kv with has_dependencies = true }
end

module Record = struct
  type t =
    { ty : Mini.Type.t option
    ; source : [ `Existing | `Generated ]
    ; scope : [ `Global | `Local ]
    ; known_value : Known_value.t option
    }
  [@@deriving fields, make]

  let is_global : t -> bool = function
    | { scope = `Global; _ } -> true
    | { scope = `Local; _ } -> false
  ;;

  let is_atomic : t -> bool = function
    | { ty = Some ty; _ } -> Mini.Type.is_atomic ty
    | { ty = None; _ } -> false
  ;;

  let was_generated : t -> bool = function
    | { source = `Generated; _ } -> true
    | { source = `Existing; _ } -> false
  ;;

  let has_dependencies (record : t) : bool =
    Option.exists (known_value record) ~f:Known_value.has_dependencies
  ;;

  let has_known_value (record : t) : bool = Option.is_some (known_value record)

  let add_dependency (record : t) : t =
    { record with
      known_value = Option.map ~f:Known_value.add_dependency record.known_value
    }
  ;;

  let erase_value (record : t) : t = { record with known_value = None }

  let make_existing_global (ty : Mini.Type.t) : t =
    make ~ty ~source:`Existing ~scope:`Global ()
  ;;

  let make_existing_local (_name : C_identifier.t) : t =
    make ~source:`Existing ~scope:`Local ()
  ;;

  let make_generated_global ?(initial_value : Value.t option)
                            (ty : Mini.Type.t) : t =
    let known_value =
      Option.map initial_value ~f:(fun value ->
          Known_value.make ~value ~has_dependencies:false)
    in
    make ~ty ~source:`Generated ~scope:`Global ?known_value ()
  ;;
end

module Map = struct
  type t = Record.t C_identifier.Map.t

  let make_existing_var_map
      (globals : Mini.Type.t C_identifier.Map.t)
      (locals : C_identifier.Set.t)
      : t
    =
    let globals_map = C_identifier.Map.map globals ~f:Record.make_existing_global in
    let locals_map = C_identifier.Set.to_map locals ~f:Record.make_existing_local in
    C_identifier.Map.merge globals_map locals_map ~f:(fun ~key ->
        ignore key;
        function
        | `Left x | `Right x | `Both (x, _) -> Some x)
  ;;

  let register_global
      ?(initial_value : Value.t option)
      (map : t)
      (key : C_identifier.t)
      (ty : Mini.Type.t)
      : t
    =
    let data = Record.make_generated_global ?initial_value ty in
    C_identifier.Map.set map ~key ~data
  ;;

  let add_dependency (map : t) ~(var : C_identifier.t) : t =
    C_identifier.Map.change map var ~f:(Option.map ~f:Record.add_dependency)
  ;;

  let erase_value_inner (map : t) ~(var : C_identifier.t) : t =
    C_identifier.Map.change map var ~f:(Option.map ~f:Record.erase_value)
  ;;

  let has_dependencies (map : t) ~(var : C_identifier.t) : bool =
    Option.exists (C_identifier.Map.find map var) ~f:Record.has_dependencies
  ;;

  let dependency_error (var : C_identifier.t) : unit Or_error.t =
    Or_error.error_s
      [%message
        "Tried to erase the known value of a depended-upon variable"
          ~var:(var : C_identifier.t)]
  ;;

  let erase_value (map : t) ~(var : C_identifier.t) : t Or_error.t =
    let open Or_error.Let_syntax in
    let%map () =
      Travesty.T_or_error.when_m (has_dependencies map ~var) ~f:(fun () ->
          dependency_error var)
    in
    erase_value_inner map ~var
  ;;

  let submap_satisfying_all (vars : t) ~(predicates : (Record.t -> bool) list) : t =
    vars |> C_identifier.Map.filter ~f:(Travesty.T_list.all ~predicates)
  ;;

  let env_satisfying_all (vars : t) ~(predicates : (Record.t -> bool) list)
      : Mini.Type.t C_identifier.Map.t
    =
    vars |> submap_satisfying_all ~predicates |> C_identifier.Map.filter_map ~f:Record.ty
  ;;

  let env_module_satisfying_all (vars : t) ~(predicates : (Record.t -> bool) list) =
    (module Mini_env.Make (struct
      let env = env_satisfying_all ~predicates vars
    end)
    : Mini_env.S)
  ;;

  let satisfying_all (vars : t) ~(predicates : (Record.t -> bool) list)
      : C_identifier.t list
    =
    vars |> submap_satisfying_all ~predicates |> C_identifier.Map.keys
  ;;

  let exists_satisfying_all (vars : t) ~(predicates : (Record.t -> bool) list) : bool =
    not (List.is_empty (satisfying_all vars ~predicates))
  ;;

  let gen_fresh_var (map : t) : C_identifier.t Quickcheck.Generator.t =
    Quickcheck.Generator.filter_map
      [%quickcheck.generator: C_identifier.Herd_safe.t]
      ~f:(fun hid ->
        let cid = C_identifier.Herd_safe.to_c_identifier hid in
        Option.some_if (not (C_identifier.Map.mem map cid)) cid)
  ;;
end
