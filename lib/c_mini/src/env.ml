(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open struct
  module Ac = Act_common
  module Q = Base_quickcheck
end

let not_found (id : Ac.C_id.t) : Error.t =
  Error.create_s
    [%message
      "Variable not found in typing environment." ~id:(id : Ac.C_id.t)]

module Record = struct
  open Q
  type t =
    { known_value: Constant.t option
    ; type_of: Type.t
    }
    [@@deriving sexp, fields, make, quickcheck]
end

type t = Record.t Map.M(Ac.C_id).t [@@deriving sexp]

let has_variables_of_basic_type (env : t) ~(basic : Type.Basic.t) : bool =
  Map.exists env ~f:(fun r -> Type.basic_type_is ~basic (Record.type_of r))

let variables_of_basic_type (env : t) ~(basic : Type.Basic.t) : t =
  Map.filter env ~f:(fun r -> Type.basic_type_is ~basic (Record.type_of r))

let record_of (env : t) ~(id:Ac.C_id.t) : Record.t Or_error.t =
  Result.of_option (Map.find env id) ~error:(not_found id)

let type_of (env : t) ~(id:Ac.C_id.t) : Type.t Or_error.t =
  Or_error.map (record_of env ~id) ~f:Record.type_of

let of_typing : Type.t Map.M(Ac.C_id).t -> t =
  Map.map ~f:(fun type_of -> Record.make ~type_of ())

let of_maps : Type.t Map.M(Ac.C_id).t -> Constant.t Map.M(Ac.C_id).t -> t =
  Map.merge ~f:(fun ~key ->
    ignore key;
    function
    | `Left type_of -> Some (Record.make ~type_of ())
    | `Both (type_of, known_value) -> Some (Record.make ~type_of ~known_value ())
    | `Right _ -> None
     )

let typing : t -> Type.t Map.M(Ac.C_id).t = Map.map ~f:Record.type_of

let known_value (env : t) ~(id : Ac.C_id.t) : Constant.t option Or_error.t =
  Or_error.map ~f:Record.known_value (record_of env ~id)

let variables_with_known_values : t ->
    (Type.t * Constant.t) Map.M(Act_common.C_id).t =
  Map.filter_map ~f:(fun (data : Record.t) ->
    Option.(data |> Record.known_value >>| fun const -> (Record.type_of data, const)))

let filter_to_known_values : t -> t =
  Map.filter ~f:(fun (data : Record.t) ->
    data |> Record.known_value |> Option.is_some)

let type_of_known_value (env : t) ~(id : Ac.C_id.t) : Type.t Or_error.t =
  Or_error.(type_of env ~id >>| Type.basic_type >>| Type.of_basic)

let gen_random_var_with_record (env : t) : Record.t Ac.C_named.t Q.Generator.t =
    (* We use a thunk here to prevent the generator from immediately
       raising an error if we try to create an empty environment. *)
    Q.Generator.Let_syntax.(
      match%bind return (Map.to_alist env) with
      | [] ->
          Error.raise_s
            [%message
              "Tried to get a random variable from an empty environment"
                ~here:[%here]]
      | xs ->
          Q.Generator.of_list (Ac.C_named.list_of_alist xs))

let gen_random_var_with_type : t -> Type.t Ac.C_named.t Q.Generator.t =
  Fn.compose (Q.Generator.map ~f:(Ac.C_named.map_right ~f:Record.type_of))
  gen_random_var_with_record

let gen_random_var : t -> Ac.C_id.t Q.Generator.t =
  Fn.compose (Q.Generator.map ~f:Ac.C_named.name)
  gen_random_var_with_record

module Random_var (E : sig val env : t end) : sig
  type t = Ac.C_id.t [@@deriving sexp_of, quickcheck]
end = struct
  type t = Ac.C_id.t [@@deriving sexp_of, quickcheck]

  let quickcheck_generator = gen_random_var E.env

  (* It's not clear whether we need a different observer here? *)
  let quickcheck_observer = Ac.C_id.quickcheck_observer

  (* Don't reduce identifiers, as this might make them no longer members of
     the environment. *)
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Random_var_with_type (E : sig val env : t end) : sig
  type t = Type.t Ac.C_named.t [@@deriving sexp_of, quickcheck]
end = struct
  type t = Type.t Ac.C_named.t [@@deriving sexp_of, quickcheck]

  let quickcheck_generator = gen_random_var_with_type E.env
  let quickcheck_observer = Ac.C_named.quickcheck_observer Type.quickcheck_observer
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Random_var_with_record (E : sig val env : t end) : sig
  type t = Record.t Ac.C_named.t [@@deriving sexp_of, quickcheck]
end = struct
  type t = Record.t Ac.C_named.t [@@deriving sexp_of, quickcheck]

  let quickcheck_generator = gen_random_var_with_record E.env
  let quickcheck_observer = Ac.C_named.quickcheck_observer Record.quickcheck_observer
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end
