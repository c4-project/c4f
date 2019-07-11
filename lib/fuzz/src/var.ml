(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Ac = Act_common
module Tx = Travesty_base_exts

module Value = struct
  type t = Int of int [@@deriving equal]
end

module Known_value = struct
  type t = {value: Value.t; has_dependencies: bool}
  [@@deriving fields, make, equal]

  let add_dependency (kv : t) : t = {kv with has_dependencies= true}
end

module Record = struct
  type t =
    { ty: Act_c_mini.Type.t option
    ; source: [`Existing | `Generated]
    ; scope: [`Global | `Local]
    ; known_value: Known_value.t option
    ; has_writes: bool [@default false] }
  [@@deriving fields, make, equal]

  let is_global : t -> bool = function
    | {scope= `Global; _} ->
        true
    | {scope= `Local; _} ->
        false

  let is_atomic : t -> bool = function
    | {ty= Some ty; _} ->
        Act_c_mini.Type.is_atomic ty
    | {ty= None; _} ->
        false

  let was_generated : t -> bool = function
    | {source= `Generated; _} ->
        true
    | {source= `Existing; _} ->
        false

  let has_dependencies (record : t) : bool =
    Option.exists (known_value record) ~f:Known_value.has_dependencies

  let has_known_value (record : t) : bool =
    Option.is_some (known_value record)

  let map_known_value (record : t) ~(f : Known_value.t -> Known_value.t) : t
      =
    {record with known_value= Option.map ~f record.known_value}

  let add_dependency : t -> t =
    map_known_value ~f:Known_value.add_dependency

  let add_write (record : t) : t = {record with has_writes= true}

  let erase_value (record : t) : t = {record with known_value= None}

  let make_existing_global (ty : Act_c_mini.Type.t) : t =
    make ~ty ~source:`Existing ~scope:`Global ()

  let make_existing_local (_name : Ac.C_id.t) : t =
    make ~source:`Existing ~scope:`Local ()

  let make_generated_global ?(initial_value : Value.t option)
      (ty : Act_c_mini.Type.t) : t =
    let known_value =
      Option.map initial_value ~f:(fun value ->
          Known_value.make ~value ~has_dependencies:false)
    in
    make ~ty ~source:`Generated ~scope:`Global ?known_value ()
end

module Map = struct
  type t = Record.t Map.M(Ac.C_id).t

  let make_existing_var_map (globals : Act_c_mini.Type.t Map.M(Ac.C_id).t)
      (locals : Ac.C_id.Set.t) : t =
    let globals_map =
      Ac.C_id.Map.map globals ~f:Record.make_existing_global
    in
    let locals_map =
      Ac.C_id.Set.to_map locals ~f:Record.make_existing_local
    in
    Ac.C_id.Map.merge globals_map locals_map ~f:(fun ~key ->
        ignore key ;
        function `Left x | `Right x | `Both (x, _) -> Some x)

  let register_global ?(initial_value : Value.t option) (map : t)
      (key : Ac.C_id.t) (ty : Act_c_mini.Type.t) : t =
    let data = Record.make_generated_global ?initial_value ty in
    Ac.C_id.Map.set map ~key ~data

  let change_var (map : t) ~(var : Ac.C_id.t) ~(f : Record.t -> Record.t) :
      t =
    Ac.C_id.Map.change map var ~f:(Option.map ~f)

  let add_write : t -> var:Ac.C_id.t -> t = change_var ~f:Record.add_write

  let add_dependency : t -> var:Ac.C_id.t -> t =
    change_var ~f:Record.add_dependency

  let erase_value_inner (map : t) ~(var : Ac.C_id.t) : t =
    Ac.C_id.Map.change map var ~f:(Option.map ~f:Record.erase_value)

  let has_dependencies (map : t) ~(var : Ac.C_id.t) : bool =
    Option.exists (Ac.C_id.Map.find map var) ~f:Record.has_dependencies

  let dependency_error (var : Ac.C_id.t) : unit Or_error.t =
    Or_error.error_s
      [%message
        "Tried to erase the known value of a depended-upon variable"
          ~var:(var : Ac.C_id.t)]

  let erase_value (map : t) ~(var : Ac.C_id.t) : t Or_error.t =
    let open Or_error.Let_syntax in
    let%map () =
      Tx.Or_error.when_m (has_dependencies map ~var) ~f:(fun () ->
          dependency_error var)
    in
    erase_value_inner map ~var

  let submap_satisfying_all (vars : t)
      ~(predicates : (Record.t -> bool) list) : t =
    vars |> Ac.C_id.Map.filter ~f:(Tx.List.all ~predicates)

  let env_satisfying_all (vars : t) ~(predicates : (Record.t -> bool) list)
      : Act_c_mini.Type.t Ac.C_id.Map.t =
    vars
    |> submap_satisfying_all ~predicates
    |> Ac.C_id.Map.filter_map ~f:Record.ty

  let env_module_satisfying_all (vars : t)
      ~(predicates : (Record.t -> bool) list) =
    ( module Act_c_mini.Env.Make (struct
      let env = env_satisfying_all ~predicates vars
    end) : Act_c_mini.Env_types.S )

  let satisfying_all (vars : t) ~(predicates : (Record.t -> bool) list) :
      Ac.C_id.t list =
    vars |> submap_satisfying_all ~predicates |> Ac.C_id.Map.keys

  let exists_satisfying_all (vars : t)
      ~(predicates : (Record.t -> bool) list) : bool =
    not (List.is_empty (satisfying_all vars ~predicates))

  let gen_fresh_var (map : t) : Ac.C_id.t Base_quickcheck.Generator.t =
    Base_quickcheck.Generator.filter_map
      [%quickcheck.generator: Ac.C_id.Herd_safe.t] ~f:(fun hid ->
        let cid = Ac.C_id.Herd_safe.to_c_identifier hid in
        Option.some_if (not (Ac.C_id.Map.mem map cid)) cid)
end
