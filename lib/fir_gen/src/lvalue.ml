(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Import

module type S = sig
  type t = Fir.Lvalue.t [@@deriving sexp_of, quickcheck]
end

module On_env (E : Fir.Env_types.S) : S =
  Fir.Lvalue.Quickcheck_generic (Fir.Env.Random_var (E))

module Values_on_env (E : Fir.Env_types.S) : sig
  type t = Fir.Lvalue.t [@@deriving sexp_of, quickcheck]
end = struct
  include On_env (E) (* to override as needed *)

  module Gen = Fir.Env.Random_var_with_type (E)

  let quickcheck_generator : t Q.Generator.t =
    Q.Generator.map Gen.quickcheck_generator
      ~f:Fir.Lvalue.on_value_of_typed_id
end

module Int_values (E : Fir.Env_types.S) : S = Values_on_env (struct
  let env =
    Fir.Env.variables_of_basic_type E.env ~basic:(Fir.Type.Basic.int ())
end)

module Bool_values (E : Fir.Env_types.S) : S = Values_on_env (struct
  let env =
    Fir.Env.variables_of_basic_type E.env ~basic:(Fir.Type.Basic.bool ())
end)
