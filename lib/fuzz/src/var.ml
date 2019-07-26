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

module Scope = struct
  type t = Local of int | Global [@@deriving compare, equal]

  let is_global : t -> bool = function Local _ -> false | Global -> true

  let of_litmus_id (id : Ac.Litmus_id.t) : t =
    match Ac.Litmus_id.tid id with Some t -> Local t | None -> Global

  let id_in_scope (scope : t) ~(id : Ac.Litmus_id.t) : bool =
    match scope with
    | Global ->
        true
    | Local from ->
        Ac.Litmus_id.is_in_scope id ~from
end

module Known_value = struct
  type t = {value: Act_c_mini.Constant.t; has_dependencies: bool}
  [@@deriving fields, make, equal]

  let add_dependency (kv : t) : t = {kv with has_dependencies= true}
end

module Record = struct
  type t =
    { ty: Act_c_mini.Type.t option
    ; source: [`Existing | `Generated]
    ; scope: Scope.t
    ; known_value: Known_value.t option
    ; has_writes: bool [@default false] }
  [@@deriving fields, make, equal]

  let is_global (r : t) : bool = Scope.is_global (scope r)

  let scope_reduce (r1 : t) (r2 : t) : t =
    if 0 <= Comparable.lift ~f:scope [%compare: Scope.t] r1 r2 then r1
    else r2

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

  let make_existing (id : Ac.Litmus_id.t) (ty : Act_c_mini.Type.t option) :
      t =
    make ~source:`Existing ~scope:(Scope.of_litmus_id id) ?ty ()

  let make_generated_global ?(initial_value : Act_c_mini.Constant.t option)
      (ty : Act_c_mini.Type.t) : t =
    let known_value =
      Option.map initial_value ~f:(fun value ->
          Known_value.make ~value ~has_dependencies:false)
    in
    make ~ty ~source:`Generated ~scope:Scope.Global ?known_value ()
end

module Map = struct
  type t = Record.t Map.M(Ac.Litmus_id).t

  let make_existing_var_map :
      Act_c_mini.Type.t option Map.M(Ac.Litmus_id).t -> t =
    Map.mapi ~f:(fun ~key ~data -> Record.make_existing key data)

  let register_global ?(initial_value : Act_c_mini.Constant.t option)
      (map : t) (id : Ac.C_id.t) (ty : Act_c_mini.Type.t) : t =
    let data = Record.make_generated_global ?initial_value ty in
    let key = Ac.Litmus_id.global id in
    Map.set map ~key ~data

  let change_var (map : t) ~(var : Ac.Litmus_id.t)
      ~(f : Record.t -> Record.t) : t =
    Map.change map var ~f:(Option.map ~f)

  let add_write : t -> var:Ac.Litmus_id.t -> t =
    change_var ~f:Record.add_write

  let add_dependency : t -> var:Ac.Litmus_id.t -> t =
    change_var ~f:Record.add_dependency

  let erase_value_inner (map : t) ~(var : Ac.Litmus_id.t) : t =
    Map.change map var ~f:(Option.map ~f:Record.erase_value)

  let has_dependencies (map : t) ~(var : Ac.Litmus_id.t) : bool =
    Option.exists (Map.find map var) ~f:Record.has_dependencies

  let dependency_error (var : Ac.Litmus_id.t) : unit Or_error.t =
    Or_error.error_s
      [%message
        "Tried to erase the known value of a depended-upon variable"
          ~var:(var : Ac.Litmus_id.t)]

  let erase_value (map : t) ~(var : Ac.Litmus_id.t) : t Or_error.t =
    Or_error.Let_syntax.(
      let%map () =
        Tx.Or_error.when_m (has_dependencies map ~var) ~f:(fun () ->
            dependency_error var)
      in
      erase_value_inner map ~var)

  let at_scope (vars : t) ~(scope : Scope.t) : Record.t Map.M(Ac.C_id).t =
    vars
    |> Map.filter_keys ~f:(fun id -> Scope.id_in_scope scope ~id)
    |> Map.to_alist
    |> Tx.Alist.map_left ~f:Ac.Litmus_id.variable_name
    |> Map.of_alist_reduce (module Ac.C_id) ~f:Record.scope_reduce

  let resolve (vars : t) ~(id : Ac.C_id.t) ~(scope : Scope.t) :
      Ac.Litmus_id.t =
    let global_id = Ac.Litmus_id.global id in
    match scope with
    | Global ->
        global_id
    | Local tid ->
        let local_id = Ac.Litmus_id.local tid id in
        if Map.mem vars local_id then local_id else global_id

  let type_submap_satisfying_all (vars : Record.t Map.M(Ac.C_id).t)
      ~(predicates : (Record.t -> bool) list) :
      Act_c_mini.Type.t Map.M(Ac.C_id).t =
    Map.filter_map vars ~f:(fun data ->
        Option.(
          data |> Record.ty >>= some_if (Tx.List.all ~predicates data)))

  let env_satisfying_all (vars : t) ~(scope : Scope.t)
      ~(predicates : (Record.t -> bool) list) :
      Act_c_mini.Type.t Map.M(Ac.C_id).t =
    vars |> at_scope ~scope |> type_submap_satisfying_all ~predicates

  let env_module_satisfying_all (vars : t) ~(scope : Scope.t)
      ~(predicates : (Record.t -> bool) list) =
    ( module Act_c_mini.Env.Make (struct
      let env = env_satisfying_all ~scope ~predicates vars
    end) : Act_c_mini.Env_types.S )

  let satisfying_all (vars : t) ~(scope : Scope.t)
      ~(predicates : (Record.t -> bool) list) : Ac.C_id.t list =
    vars |> env_satisfying_all ~scope ~predicates |> Map.keys

  let exists_satisfying_all (vars : t) ~(scope : Scope.t)
      ~(predicates : (Record.t -> bool) list) : bool =
    not (Map.is_empty (env_satisfying_all vars ~scope ~predicates))

  let cid_exists (vars : t) ~(cid : Ac.C_id.t) : bool =
    Map.existsi vars ~f:(fun ~key ~data ->
        ignore data ;
        [%equal: Ac.C_id.t] cid (Ac.Litmus_id.variable_name key))

  let gen_fresh_var (map : t) : Ac.C_id.t Base_quickcheck.Generator.t =
    Base_quickcheck.Generator.filter_map
      [%quickcheck.generator: Ac.C_id.Herd_safe.t] ~f:(fun hid ->
        let cid = Ac.C_id.Herd_safe.to_c_identifier hid in
        Option.some_if (not (cid_exists map ~cid)) cid)
end
