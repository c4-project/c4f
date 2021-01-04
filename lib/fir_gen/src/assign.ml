(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

module type S = sig
  type t = Fir.Assign.t [@@deriving sexp_of, quickcheck]
end

module Int (Src : Fir.Env_types.S) (Dst : Fir.Env_types.S) : S =
  Fir.Assign.Quickcheck_generic
    (Fir.Assign.Source.Quickcheck_int
       (Expr.Int_values (Src))) (Lvalue.Int_values (Dst))

module Bool (Src : Fir.Env_types.S) (Dst : Fir.Env_types.S) : S =
  Fir.Assign.Quickcheck_generic
    (Fir.Assign.Source.Quickcheck_bool
       (Expr.Bool_values (Src))) (Lvalue.Bool_values (Dst))

let int ~(src : Fir.Env.t) ~(dst : Fir.Env.t) :
    Fir.Assign.t Q.Generator.t option =
  (* Can't use prim_type here as we need the source variables to be
     non-atomic. *)
  if Fir.Env.has_vars_of_basic_type src ~basic:(Fir.Type.Basic.int ()) then
    Some
      (let module I =
         Int
           (struct
             let env = src
           end)
           (struct
             let env = dst
           end)
       in
      I.quickcheck_generator)
  else None

let bool ~(src : Fir.Env.t) ~(dst : Fir.Env.t) :
    Fir.Assign.t Q.Generator.t option =
  (* Ditto. *)
  if Fir.Env.has_vars_of_basic_type src ~basic:(Fir.Type.Basic.bool ()) then
    Some
      (let module B =
         Bool
           (struct
             let env = src
           end)
           (struct
             let env = dst
           end)
       in
      B.quickcheck_generator)
  else None

let any ~(src : Fir.Env.t) ~(dst : Fir.Env.t) :
    Fir.Assign.t Q.Generator.t option =
  (* TODO(@MattWindsor91): there must be an easier way to do this. *)
  Option.map
    (Option.all [int ~src ~dst; bool ~src ~dst])
    ~f:Q.Generator.union
