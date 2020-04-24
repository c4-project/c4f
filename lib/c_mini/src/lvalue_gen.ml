(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module L = Lvalue

module On_env (E : Env_types.S) : sig
  type t = L.t [@@deriving sexp_of, quickcheck]
end =
  L.Quickcheck_generic (Env.Random_var (E))

module Typed_values_on_env (T : sig
  val basic_type : Type.Basic.t
end)
(E : Env_types.S) : sig
  type t = L.t [@@deriving sexp_of, quickcheck]
end = struct
  open Base_quickcheck
  include On_env (E) (* to override as needed *)

  module Gen = Env.Random_var_with_type (E)

  let quickcheck_generator : t Generator.t =
    Generator.filter_map Gen.quickcheck_generator ~f:(fun r ->
        let id = Act_common.C_named.name r in
        let ty = Act_common.C_named.value r in
        Option.some_if
          (Type.basic_type_is ~basic:T.basic_type ty)
          (L.on_value_of_typed_id ~id ~ty))
end

module Int_values (E : Env_types.S) : sig
  type t = L.t [@@deriving sexp_of, quickcheck]
end =
  Typed_values_on_env
    (struct
      let basic_type = Type.Basic.int ()
    end)
    (E)

module Bool_values (E : Env_types.S) : sig
  type t = L.t [@@deriving sexp_of, quickcheck]
end =
  Typed_values_on_env
    (struct
      let basic_type = Type.Basic.bool ()
    end)
    (E)
