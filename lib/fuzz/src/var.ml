(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module Acc = Accessor_base
  module Ac = Act_common
  module Tx = Travesty_base_exts
end

module Record = struct
  module Access = struct
    type t =
      { env_record: Act_fir.Env.Record.t
      ; source: [`Existing | `Generated]
      ; scope: Ac.Scope.t
      ; has_dependencies: bool [@default false]
      ; has_writes: bool [@default false] }
    [@@deriving accessors, make, equal]

    let type_of = [%accessor Acc.(env_record @> Act_fir.Env.Record.type_of)]

    let known_value =
      [%accessor Acc.(env_record @> Act_fir.Env.Record.known_value)]
  end

  include Access

  let type_of = Acc.get Access.type_of

  let scope = Acc.get Access.scope

  let known_value = Acc.get_option Access.known_value

  let has_dependencies = Acc.get Access.has_dependencies

  let has_writes = Acc.get Access.has_writes

  let env_record : t -> Act_fir.Env.Record.t = Acc.get Access.env_record

  let is_global (r : t) : bool = Ac.Scope.is_global (scope r)

  let was_generated : t -> bool = function
    | {source= `Generated; _} ->
        true
    | {source= `Existing; _} ->
        false

  let has_known_value (record : t) : bool =
    Option.is_some (known_value record)

  let try_get_known_value (record : t) : Act_fir.Constant.t Or_error.t =
    Result.of_option (known_value record)
      ~error:(Error.of_string "No known value for this record.")

  let add_dependency : t -> t = Acc.set Access.has_dependencies ~to_:true

  let add_write : t -> t = Acc.set Access.has_writes ~to_:true

  let erase_value : t -> t =
    Acc.(set (Access.env_record @> Act_fir.Env.Record.known_value_opt))
      ~to_:None

  let make_existing (scope : Ac.Scope.t) (type_of : Act_fir.Type.t) : t =
    make ~source:`Existing ~scope
      ~env_record:(Act_fir.Env.Record.make ~type_of ())
      ()

  let make_generated ?(initial_value : Act_fir.Constant.t option)
      (scope : Ac.Scope.t) (type_of : Act_fir.Type.t) : t =
    make ~source:`Generated ~scope
      ~env_record:
        (Act_fir.Env.Record.make ~type_of ?known_value:initial_value ())
      ()
end

module Map = struct
  type t = Record.t Ac.Scoped_map.t

  let make_existing_record (id : Ac.Litmus_id.t) (ty : Act_fir.Type.t) :
      Record.t =
    Record.make_existing (Ac.Litmus_id.scope id) ty

  let make_existing_var_map (test : Act_fir.Litmus.Test.t) : t Or_error.t =
    Or_error.(
      test |> Act_fir.Litmus_vars.make_type_alist
      >>| List.map ~f:(fun (id, ty) -> (id, make_existing_record id ty))
      >>= Act_common.Scoped_map.of_litmus_id_alist)

  let register_var ?(initial_value : Act_fir.Constant.t option) (map : t)
      (id : Ac.Litmus_id.t) (ty : Act_fir.Type.t) : t =
    let scope = Ac.Litmus_id.scope id in
    let record = Record.make_generated ?initial_value scope ty in
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
      ~(predicates : (Record.t -> bool) list) : Act_fir.Env.t =
    vars
    |> records_satisfying_all ~scope ~predicates
    |> Map.map ~f:Record.env_record

  let satisfying_all (vars : t) ~(scope : Ac.Scope.t)
      ~(predicates : (Record.t -> bool) list) : Ac.C_id.t list =
    vars |> records_satisfying_all ~scope ~predicates |> Map.keys

  let exists_satisfying_all (vars : t) ~(scope : Ac.Scope.t)
      ~(predicates : (Record.t -> bool) list) : bool =
    not (Map.is_empty (records_satisfying_all vars ~scope ~predicates))

  let scopes_with_vars (vars : t) ~(predicates : (Record.t -> bool) list) :
      Set.M(Ac.Scope).t =
    vars
    |> Ac.Scoped_map.filter ~f:(Tx.List.all ~predicates)
    |> Ac.Scoped_map.to_litmus_id_map |> Map.keys
    |> List.map ~f:Ac.Litmus_id.scope
    |> Set.of_list (module Ac.Scope)

  let gen_fresh_var (map : t) : Ac.C_id.t Base_quickcheck.Generator.t =
    Base_quickcheck.Generator.filter [%quickcheck.generator: Ac.C_id.Human.t]
      ~f:(fun id -> not (Ac.Scoped_map.c_id_mem map ~id))
end
