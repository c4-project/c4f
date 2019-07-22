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
  L.Quickcheck_generic (E.Random_var)

module Typed_values_on_env (T : sig
  val basic_type : Type.Basic.t
end)
(E : Env_types.S) : sig
  type t = L.t [@@deriving sexp_of, quickcheck]
end = struct
  open Base_quickcheck
  include On_env (E) (* to override as needed *)

  let quickcheck_generator : t Generator.t =
    Generator.map
      (Generator.of_list
         (Map.to_alist (E.variables_of_basic_type T.basic_type)))
      ~f:(fun (id, ty) -> L.on_value_of_typed_id ~id ~ty)
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
