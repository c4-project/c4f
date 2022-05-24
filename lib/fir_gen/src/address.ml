(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Import

module type S = sig
  type t = Fir.Address.t [@@deriving sexp_of, quickcheck]
end

module On_env (E : Fir.Env_types.S) : S =
  Fir.Address.Quickcheck_generic (Lvalue.On_env (E))

module Pointers_on_env (E : Fir.Env_types.S) : S = struct
  include On_env (E) (* to override as needed *)

  module Gen = Fir.Env.Random_var_with_type (E)

  let quickcheck_generator : t Q.Generator.t =
    Q.Generator.map Gen.quickcheck_generator
      ~f:Fir.Address.on_address_of_typed_id
end

module Atomic_int_pointers (E : Fir.Env_types.S) : S =
Pointers_on_env (struct
  let env =
    Fir.Env.variables_of_basic_type E.env
      ~basic:(Fir.Type.Basic.int ~is_atomic:true ())
end)

module Atomic_bool_pointers (E : Fir.Env_types.S) : S =
Pointers_on_env (struct
  let env =
    Fir.Env.variables_of_basic_type E.env
      ~basic:(Fir.Type.Basic.bool ~is_atomic:true ())
end)
