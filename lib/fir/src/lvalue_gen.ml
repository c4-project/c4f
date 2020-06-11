(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

module On_env (E : Env_types.S) : sig
  type t = Lvalue.t [@@deriving sexp_of, quickcheck]
end =
  Lvalue.Quickcheck_generic (Env.Random_var (E))

module Values_on_env (E : Env_types.S) : sig
  type t = Lvalue.t [@@deriving sexp_of, quickcheck]
end = struct
  open Base_quickcheck
  include On_env (E) (* to override as needed *)

  module Gen = Env.Random_var_with_type (E)

  let quickcheck_generator : t Generator.t =
    Generator.map Gen.quickcheck_generator ~f:Lvalue.on_value_of_typed_id
end

module Int_values (E : Env_types.S) : sig
  type t = Lvalue.t [@@deriving sexp_of, quickcheck]
end = Values_on_env (struct
  let env = Env.variables_of_basic_type E.env ~basic:(Type.Basic.int ())
end)

module Bool_values (E : Env_types.S) : sig
  type t = Lvalue.t [@@deriving sexp_of, quickcheck]
end = Values_on_env (struct
  let env = Env.variables_of_basic_type E.env ~basic:(Type.Basic.bool ())
end)
