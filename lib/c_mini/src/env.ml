(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Ac = Act_common

module Make (E : sig
  val env : Type.t Map.M(Act_common.C_id).t
end) =
struct
  let env = E.env

  module Random_var : sig
    type t = Ac.C_id.t [@@deriving sexp_of, quickcheck]
  end = struct
    open Base_quickcheck

    type t = Ac.C_id.t

    let sexp_of_t = Ac.C_id.sexp_of_t

    let quickcheck_generator : Ac.C_id.t Generator.t =
      (* We use a thunk here to prevent the generator from immediately
         raising an error if we try to create an empty environment. *)
      Generator.Let_syntax.(
        match%bind return (Map.keys E.env) with
        | [] ->
            Error.raise_s
              [%message
                "Tried to get a random variable from an empty environment"
                  ~here:[%here]]
        | xs ->
            Generator.of_list xs)

    (* It's not clear whether we need a different observer here? *)
    let quickcheck_observer = Ac.C_id.quickcheck_observer

    (* Don't reduce identifiers, as this might make them no longer members
       of the environment. *)
    let quickcheck_shrinker = Shrinker.atomic
  end

  let has_variables_of_basic_type (basic : Type.Basic.t) : bool =
    Map.exists E.env ~f:Type.(basic_type_is ~basic)

  let variables_of_basic_type (basic : Type.Basic.t) :
      Type.t Map.M(Ac.C_id).t =
    Map.filter E.env ~f:Type.(basic_type_is ~basic)
end

module Make_with_known_values (E : sig
  val env : Type.t Map.M(Act_common.C_id).t

  val known_values : Set.M(Constant).t Map.M(Act_common.C_id).t
end) : Env_types.S_with_known_values = struct
  include Make (E)

  let known_values : Act_common.C_id.t -> Set.M(Constant).t option =
    Map.find E.known_values
end
