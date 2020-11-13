(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

type 'e t = {if_: 'e; then_: 'e; else_: 'e}
[@@deriving sexp, accessors, compare, equal, quickcheck]

(* Not exported because its signature is confusing. *)
let make (if_ : 'e) (then_ : 'e) (else_ : 'e) : 'e t = {if_; then_; else_}

let exprs : ('i -> 'e1 -> 'e2, 'i -> 'e1 t -> 'e2 t, [< many]) Accessor.t =
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
  Types.S_type_checker with type t := Type.t t = struct
  let type_of ({if_; then_; else_} : Type.t t) : Type.t Or_error.t =
    Or_error.(
      Let_syntax.(
        let%bind () =
          Tx.Or_error.unless_m
            (Type.basic_type_is if_ ~basic:(Type.Basic.bool ()))
            ~f:(fun () ->
              error_s
                [%message
                  "Condition of ternary statement must be boolean value"
                    ~actual_type:(if_ : Type.t)])
        in
        Type.check then_ else_))
end
