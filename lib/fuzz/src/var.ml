(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

module Record = struct
  module Access = struct
    type t =
      { env_record: Fir.Env.Record.t
      ; source: [`Existing | `Generated]
      ; scope: Common.Scope.t
      ; has_dependencies: bool [@default false]
      ; has_writes: bool [@default false] }
    [@@deriving accessors, make, equal]

    let type_of = [%accessor env_record @> Fir.Env.Record.type_of]

    let known_value = [%accessor env_record @> Fir.Env.Record.known_value]

    let pp_source (f : Formatter.t) (x : t) : unit =
      ( match x.source with
      | `Existing ->
          Fmt.(styled `Red (any "existing"))
      | `Generated ->
          Fmt.(styled `Green (any "generated")) )
        f ()

    let pp_ty : t Fmt.t =
      Fmt.(
        using
          (Accessor.get (env_record @> Fir.Env.Record.type_of))
          (of_to_string Fir.Type.to_string))

    let pp_kv : t Fmt.t =
      Fmt.(
        using
          (Accessor.get (env_record @> Fir.Env.Record.known_value_opt))
          (any "=" ++ option ~none:(any "?") Fir.Constant.pp))

    let pp_scope : t Fmt.t =
      Fmt.(using (Accessor.get scope) (any "@@" ++ Common.Scope.pp))

    let flag_str (x : t) : string =
      String.concat ~sep:";"
        (List.filter_opt
           [ Option.some_if x.has_dependencies "Dep"
           ; Option.some_if x.has_writes "Write" ])

    let pp : t Fmt.t =
      Fmt.concat ~sep:Fmt.comma
        [ pp_ty
        ; pp_kv
        ; pp_scope
        ; pp_source
        ; Fmt.brackets (Fmt.of_to_string flag_str) ]
  end

  include Access

  let type_of = Accessor.get Access.type_of

  let scope = Accessor.get Access.scope

  let known_value = Accessor.get_option Access.known_value

  let has_dependencies = Accessor.get Access.has_dependencies

  let has_writes = Accessor.get Access.has_writes

  let env_record : t -> Fir.Env.Record.t = Accessor.get Access.env_record

  let is_global (r : t) : bool = Common.Scope.is_global (scope r)

  let was_generated : t -> bool = function
    | {source= `Generated; _} ->
        true
    | {source= `Existing; _} ->
        false

  let can_safely_modify : t -> bool =
    (* We don't know whether variables that existed before fuzzing have any
       dependencies, as we don't do any flow analysis of them. Maybe one day
       this will be relaxed? *)
    Tx.Fn.(Fn.non has_dependencies &&& was_generated)

  let has_known_value (record : t) : bool =
    Option.is_some (known_value record)

  let try_get_known_value (record : t) : Fir.Constant.t Or_error.t =
    Result.of_option (known_value record)
      ~error:(Error.of_string "No known value for this record.")

  let add_dependency (x : t) : t = x.@(Access.has_dependencies) <- true

  let add_write (x : t) : t = x.@(Access.has_writes) <- true

  let erase_value (x : t) : t =
    x.@(Access.env_record @> Fir.Env.Record.known_value_opt) <- None

  let make_existing (scope : Common.Scope.t) (type_of : Fir.Type.t) : t =
    make ~source:`Existing ~scope
      ~env_record:(Fir.Env.Record.make ~type_of ())
      ()

  let make_generated ?(initial_value : Fir.Constant.t option)
      (scope : Common.Scope.t) (type_of : Fir.Type.t) : t =
    make ~source:`Generated ~scope
      ~env_record:
        (Fir.Env.Record.make ~type_of ?known_value:initial_value ())
      ()
end

module Map = struct
  type t = Record.t Common.Scoped_map.t

  let make_existing_record (id : Common.Litmus_id.t) (ty : Fir.Type.t) :
      Record.t =
    Record.make_existing (Common.Litmus_id.scope id) ty

  let make_existing_var_map (test : Fir.Litmus.Test.t) : t Or_error.t =
    Or_error.(
      test |> Fir.Litmus.Var.make_alist
      >>| List.map ~f:(fun (id, {Fir.Litmus.Var.Record.ty; _}) ->
              (id, make_existing_record id ty))
      >>= Act_common.Scoped_map.of_litmus_id_alist)

  let register_var ?(initial_value : Fir.Constant.t option) (map : t)
      (id : Common.Litmus_id.t) (ty : Fir.Type.t) : t =
    let scope = Common.Litmus_id.scope id in
    let record = Record.make_generated ?initial_value scope ty in
    Common.Scoped_map.set map ~id ~record

  let add_write : t -> id:Common.Litmus_id.t -> t =
    Common.Scoped_map.map_record ~f:Record.add_write

  let add_dependency : t -> id:Common.Litmus_id.t -> t =
    Common.Scoped_map.map_record ~f:Record.add_dependency

  let erase_value_inner : t -> id:Common.Litmus_id.t -> t =
    Common.Scoped_map.map_record ~f:Record.erase_value

  let has_dependencies (map : t) ~(id : Common.Litmus_id.t) : bool =
    match Common.Scoped_map.find_by_litmus_id map ~id with
    | Ok r ->
        Record.has_dependencies r
    | _ ->
        false

  let dependency_error (var : Common.Litmus_id.t) : unit Or_error.t =
    Or_error.error_s
      [%message
        "Tried to erase the known value of a depended-upon variable"
          ~var:(var : Common.Litmus_id.t)]

  let erase_value (map : t) ~(id : Common.Litmus_id.t) : t Or_error.t =
    Or_error.Let_syntax.(
      let%map () =
        Tx.Or_error.when_m (has_dependencies map ~id) ~f:(fun () ->
            dependency_error id)
      in
      erase_value_inner map ~id)

  let records_satisfying_all (vars : t) ~(scope : Common.Scope.t)
      ~(predicates : (Record.t -> bool) list) : Record.t Map.M(Common.C_id).t
      =
    vars
    |> Common.Scoped_map.to_c_id_map ~scope
    |> Map.filter ~f:(Tx.List.all ~predicates)

  let env_satisfying_all (vars : t) ~(scope : Common.Scope.t)
      ~(predicates : (Record.t -> bool) list) : Fir.Env.t =
    vars
    |> records_satisfying_all ~scope ~predicates
    |> Map.map ~f:Record.env_record

  let satisfying_all (vars : t) ~(scope : Common.Scope.t)
      ~(predicates : (Record.t -> bool) list) : Common.C_id.t list =
    vars |> records_satisfying_all ~scope ~predicates |> Map.keys

  let exists_satisfying_all (vars : t) ~(scope : Common.Scope.t)
      ~(predicates : (Record.t -> bool) list) : bool =
    not (Map.is_empty (records_satisfying_all vars ~scope ~predicates))

  let scopes_with_vars (vars : t) ~(predicates : (Record.t -> bool) list) :
      Set.M(Common.Scope).t =
    vars
    |> Common.Scoped_map.filter ~f:(Tx.List.all ~predicates)
    |> Common.Scoped_map.to_litmus_id_map |> Map.keys
    |> List.map ~f:Common.Litmus_id.scope
    |> Set.of_list (module Common.Scope)

  let threads_of : Set.M(Common.Scope).t -> Set.M(Int).t =
    Accessor.Set.of_accessor
      (module Int)
      (Accessor.Set.each @> Common.Scope.local)

  let threads_with_vars (vars : t) ~(predicates : (Record.t -> bool) list) :
      [`All | `These of Set.M(Int).t] =
    let scopes = scopes_with_vars vars ~predicates in
    (* If the global scope has at least one variable matching the predicates,
       all threads have matching variables. *)
    if Set.mem scopes Global then `All else `These (threads_of scopes)

  let gen_fresh_var' (exists : Common.C_id.t -> bool) :
      Common.C_id.t Base_quickcheck.Generator.t =
    Base_quickcheck.Generator.filter
      [%quickcheck.generator: Common.C_id.Human.t] ~f:(Fn.non exists)

  let gen_fresh_var (map : t) : Common.C_id.t Base_quickcheck.Generator.t =
    gen_fresh_var' (fun id -> Common.Scoped_map.c_id_mem map ~id)

  let gen_fresh_vars (map : t) ~(n : int) :
      Common.C_id.t list Base_quickcheck.Generator.t =
    let existing =
      Common.Scoped_map.build_set
        (module Common.C_id)
        ~f:(fun k _ -> Some (Common.Litmus_id.variable_name k))
        map
    in
    Base_quickcheck.Generator.(
      Sequence.fold_m (Sequence.init n ~f:Fn.id)
        ~init:(existing, []) ~bind ~return ~f:(fun (existing, vs) _ ->
          gen_fresh_var' (Set.mem existing)
          >>| fun v -> (Set.add existing v, v :: vs))
      >>| snd)
end
