(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

let not_found (id : Common.C_id.t) : Error.t =
  Error.create_s
    [%message
      "Variable not found in typing environment." ~id:(id : Common.C_id.t)]

module Record = struct
  open Q

  type t = {known_value: Constant.t option; type_of: Type.t}
  [@@deriving equal, sexp, accessors, make, quickcheck]

  let known_value_opt = known_value

  let known_value :
      type a. (a, Constant.t, t, [< Accessor.optional]) Accessor.Simple.t =
    [%accessor Accessor.(known_value_opt @> Option.some)]
end

type t = Record.t Map.M(Common.C_id).t [@@deriving sexp]

let basic_type = [%accessor Record.type_of @> Type.Access.basic_type]

let prim_type = [%accessor basic_type @> Type.Basic.Access.prim]

let has_vars_of_prim_type (env : t) ~(prim : Type.Prim.t) : bool =
  Map.exists env ~f:(Type.Prim.eq prim_type ~to_:prim)

let variables_of_prim_type (env : t) ~(prim : Type.Prim.t) : t =
  Map.filter env ~f:(Type.Prim.eq prim_type ~to_:prim)

let has_vars_of_basic_type (env : t) ~(basic : Type.Basic.t) : bool =
  Map.exists env ~f:(Type.Basic.eq basic_type ~to_:basic)

let variables_of_basic_type (env : t) ~(basic : Type.Basic.t) : t =
  Map.filter env ~f:(Type.Basic.eq basic_type ~to_:basic)

let or_not_found (id : Common.C_id.t) : 'a option -> 'a Or_error.t =
  Result.of_option ~error:(not_found id)

let get_or_not_found (type a) (acc : ('i, a, t, _) Accessor.Simple.t)
    (env : t) ~(id : Common.C_id.t) : a Or_error.t =
  env.@?(acc) |> or_not_found id

let record_of (env : t) ~(id : Common.C_id.t) : Record.t Or_error.t =
  get_or_not_found (Accessor.Map.found id) env ~id

let type_of (env : t) ~(id : Common.C_id.t) : Type.t Or_error.t =
  get_or_not_found (Accessor.Map.found id @> Record.type_of) env ~id

let known_value (env : t) ~(id : Common.C_id.t) :
    Constant.t option Or_error.t =
  get_or_not_found (Accessor.Map.found id @> Record.known_value_opt) env ~id

let of_typing : Type.t Map.M(Common.C_id).t -> t =
  Map.map ~f:(fun type_of -> Record.make ~type_of ())

let of_maps :
    Type.t Map.M(Common.C_id).t -> Constant.t Map.M(Common.C_id).t -> t =
  Map.merge ~f:(fun ~key ->
      ignore key ;
      function
      | `Left type_of ->
          Some (Record.make ~type_of ())
      | `Both (type_of, known_value) ->
          Some (Record.make ~type_of ~known_value ())
      | `Right _ ->
          None)

let typing : t -> Type.t Map.M(Common.C_id).t =
  Map.map ~f:(Accessor.get Record.type_of)

let variables_with_known_values :
    t -> (Type.t * Constant.t) Map.M(Act_common.C_id).t =
  Map.filter_map ~f:(fun (data : Record.t) ->
      Option.Let_syntax.(
        let%map kv = data.@?(Record.known_value) in
        let ty = data.type_of in
        (ty, kv)))

let filter_to_known_values : t -> t =
  Map.filter ~f:(fun (data : Record.t) ->
      not (Accessor.is_empty Record.known_value data))

let type_of_known_value (env : t) ~(id : Common.C_id.t) : Type.t Or_error.t =
  Or_error.(type_of env ~id >>| Type.basic_type >>| Type.make)

let gen_random_var_with_record (env : t) :
    Record.t Common.C_named.t Q.Generator.t =
  (* We use a thunk here to prevent the generator from immediately raising an
     error if we try to create an empty environment. *)
  Q.Generator.Let_syntax.(
    match%bind return (Map.to_alist env) with
    | [] ->
        Error.raise_s
          [%message
            "Tried to get a random variable from an empty environment"
              ~here:[%here]]
    | xs ->
        Q.Generator.of_list (Common.C_named.list_of_alist xs))

let gen_random_var_with_type : t -> Type.t Common.C_named.t Q.Generator.t =
  Fn.compose
    (Q.Generator.map
       ~f:(Common.C_named.map_right ~f:(Accessor.get Record.type_of)))
    gen_random_var_with_record

let gen_random_var : t -> Common.C_id.t Q.Generator.t =
  Fn.compose
    (Q.Generator.map ~f:(Accessor.get Common.C_named.name))
    gen_random_var_with_record

module Random_var (E : sig
  val env : t
end) : sig
  type t = Common.C_id.t [@@deriving sexp_of, quickcheck]
end = struct
  type t = Common.C_id.t [@@deriving sexp_of, quickcheck]

  let quickcheck_generator = gen_random_var E.env

  (* It's not clear whether we need a different observer here? *)
  let quickcheck_observer = Common.C_id.quickcheck_observer

  (* Don't reduce identifiers, as this might make them no longer members of
     the environment. *)
  let quickcheck_shrinker = Q.Shrinker.atomic
end

module Random_var_with_type (E : sig
  val env : t
end) : sig
  type t = Type.t Common.C_named.t [@@deriving sexp_of, quickcheck]
end = struct
  type t = Type.t Common.C_named.t [@@deriving sexp_of, quickcheck]

  let quickcheck_generator = gen_random_var_with_type E.env

  let quickcheck_observer =
    Common.C_named.quickcheck_observer Type.quickcheck_observer

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Random_var_with_record (E : sig
  val env : t
end) : sig
  type t = Record.t Common.C_named.t [@@deriving sexp_of, quickcheck]
end = struct
  type t = Record.t Common.C_named.t [@@deriving sexp_of, quickcheck]

  let quickcheck_generator = gen_random_var_with_record E.env

  let quickcheck_observer =
    Common.C_named.quickcheck_observer Record.quickcheck_observer

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end
