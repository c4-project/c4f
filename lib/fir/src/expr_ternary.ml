(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(* Needed because Base shadows it: *)
module Ty = Type

open Base
open Import

type 'e t = {if_: 'e; then_: 'e; else_: 'e}
[@@deriving sexp, accessors, compare, equal, quickcheck]

(* Not exported because its signature is confusing. *)
let make (if_ : 'e) (then_ : 'e) (else_ : 'e) : 'e t = {if_; then_; else_}

let exprs :
    ('i -> 'e1 -> 'e2, 'i -> 'e1 t -> 'e2 t, [< many]) Accessor.General.t =
  [%accessor
    Accessor.(
      many
        Many.(
          fun {if_; then_; else_} ->
            map3 (access if_) (access then_) (access else_) ~f:make))]

let quickcheck_generator_ite ~(gen_if : 'e Q.Generator.t)
    ~(gen_then : 'e Q.Generator.t) ~(gen_else : 'e Q.Generator.t) :
    'e t Q.Generator.t =
  Q.Generator.map3 gen_if gen_then gen_else ~f:make

module Type_check (E : Env_types.S) :
  Types.S_type_checker with type t := Ty.t t = struct
  let type_of ({if_; then_; else_} : Ty.t t) : Ty.t Or_error.t =
    Or_error.(
      Let_syntax.(
        let%bind () =
          Tx.Or_error.unless_m
            (Ty.basic_type_is if_ ~basic:(Ty.Basic.bool ()))
            ~f:(fun () ->
              error_s
                [%message
                  "Condition of ternary statement must be boolean value"
                    ~actual_type:(if_ : Ty.t)] )
        in
        Ty.check then_ else_))
end
