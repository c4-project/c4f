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

module Known_value = struct
  type t = {value: Act_c_mini.Constant.t; has_dependencies: bool}
  [@@deriving fields, make, equal]

  let add_dependency (kv : t) : t = {kv with has_dependencies= true}
end

module Record = struct
  type t =
    { ty: Act_c_mini.Type.t
    ; source: [`Existing | `Generated]
    ; scope: Ac.Scope.t
    ; known_value: Known_value.t option
    ; has_writes: bool [@default false] }
  [@@deriving fields, make, equal]

  let is_global (r : t) : bool = Ac.Scope.is_global (scope r)

  let has_basic_type (r : t) ~(basic : Act_c_mini.Type.Basic.t) : bool =
    Act_c_mini.Type.basic_type_is ~basic (ty r)

  let is_atomic (r : t) : bool = Act_c_mini.Type.is_atomic (ty r)

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

  let add_dependency : t -> t = map_known_value ~f:Known_value.add_dependency

  let add_write (record : t) : t = {record with has_writes= true}

  let erase_value (record : t) : t = {record with known_value= None}

  let make_existing (scope : Ac.Scope.t) (ty : Act_c_mini.Type.t) : t =
    make ~source:`Existing ~scope ~ty ()

  let make_generated_global ?(initial_value : Act_c_mini.Constant.t option)
      (ty : Act_c_mini.Type.t) : t =
    let known_value =
      Option.map initial_value ~f:(fun value ->
          Known_value.make ~value ~has_dependencies:false)
    in
    make ~ty ~source:`Generated ~scope:Ac.Scope.Global ?known_value ()
end

module Map = struct
  type t = Record.t Ac.Scoped_map.t

  let make_existing_var_map (test : Act_c_mini.Litmus.Test.t) : t Or_error.t
      =
    Act_c_mini.Litmus_vars.make_scoped_map test
      ~make_global:(fun _ ty ->
        Or_error.return (Record.make_existing Global ty))
      ~make_local:(fun tid _ ty ->
        Or_error.return (Record.make_existing (Local tid) ty))

  let register_global ?(initial_value : Act_c_mini.Constant.t option)
      (map : t) (id : Ac.C_id.t) (ty : Act_c_mini.Type.t) : t =
    let record = Record.make_generated_global ?initial_value ty in
    let id = Ac.Litmus_id.global id in
    Ac.Scoped_map.set map ~id ~record

  let add_write : t -> id:Ac.Litmus_id.t -> t =
    Ac.Scoped_map.map_record ~f:Record.add_write

  let add_dependency : t -> id:Ac.Litmus_id.t -> t =
    Ac.Scoped_map.map_record ~f:Record.add_dependency

  let erase_value_inner : t -> id:Ac.Litmus_id.t -> t =
    Ac.Scoped_map.map_record ~f:Record.erase_value

  let has_dependencies (map : t) ~(id : Ac.Litmus_id.t) : bool =
    match Ac.Scoped_map.find_by_litmus_id map ~id with
    | Ok r ->
        Record.has_dependencies r
    | _ ->
        false

  let dependency_error (var : Ac.Litmus_id.t) : unit Or_error.t =
    Or_error.error_s
      [%message
        "Tried to erase the known value of a depended-upon variable"
          ~var:(var : Ac.Litmus_id.t)]

  let erase_value (map : t) ~(id : Ac.Litmus_id.t) : t Or_error.t =
    Or_error.Let_syntax.(
      let%map () =
        Tx.Or_error.when_m (has_dependencies map ~id) ~f:(fun () ->
            dependency_error id)
      in
      erase_value_inner map ~id)

  let records_satisfying_all (vars : t) ~(scope : Ac.Scope.t)
      ~(predicates : (Record.t -> bool) list) : Record.t Map.M(Ac.C_id).t =
    vars
    |> Ac.Scoped_map.to_c_id_map ~scope
    |> Map.filter ~f:(Tx.List.all ~predicates)

  let env_satisfying_all (vars : t) ~(scope : Ac.Scope.t)
      ~(predicates : (Record.t -> bool) list) =
    vars |> records_satisfying_all ~scope ~predicates |> Map.map ~f:Record.ty

  let env_module_satisfying_all (vars : t) ~(scope : Ac.Scope.t)
      ~(predicates : (Record.t -> bool) list) :
      (module Act_c_mini.Env_types.S) =
    ( module Act_c_mini.Env.Make (struct
      let env = env_satisfying_all vars ~scope ~predicates
    end) )

  let kv (r : Record.t) : Act_c_mini.Constant.t option =
    Option.(r |> Record.known_value >>| Known_value.value)

  let env_module_with_known_values
      ?(predicates : (Record.t -> bool) list = []) (vars : t)
      ~(scope : Ac.Scope.t) :
      (module Act_c_mini.Env_types.S_with_known_values) =
    ( module Act_c_mini.Env.Make_with_known_values (struct
      let records = records_satisfying_all vars ~scope ~predicates

      let env = Map.map ~f:Record.ty records

      let known_values = Map.filter_map ~f:kv records
    end) )

  let satisfying_all (vars : t) ~(scope : Ac.Scope.t)
      ~(predicates : (Record.t -> bool) list) : Ac.C_id.t list =
    vars |> records_satisfying_all ~scope ~predicates |> Map.keys

  let exists_satisfying_all (vars : t) ~(scope : Ac.Scope.t)
      ~(predicates : (Record.t -> bool) list) : bool =
    not (Map.is_empty (records_satisfying_all vars ~scope ~predicates))

  let gen_fresh_var (map : t) : Ac.C_id.t Base_quickcheck.Generator.t =
    Base_quickcheck.Generator.filter_map
      [%quickcheck.generator: Ac.C_id.Herd_safe.t] ~f:(fun hid ->
        let id = Ac.C_id.Herd_safe.to_c_identifier hid in
        Option.some_if (not (Ac.Scoped_map.c_id_mem map ~id)) id)
end
