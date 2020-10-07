(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
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

let any ~(src : Fir.Env.t) ~(dst : Fir.Env.t) : Fir.Assign.t Q.Generator.t =
  (* TODO(@MattWindsor91): there must be an easier way to do this. *)
  let module Src = struct
    let env = src
  end in
  let module Dst = struct
    let env = dst
  end in
  (* Can't use prim_type here as we need the source variables to be
     non-atomic. *)
  let has_int =
    Fir.Env.has_vars_of_basic_type src ~basic:(Fir.Type.Basic.int ())
  in
  let has_bool =
    Fir.Env.has_vars_of_basic_type src ~basic:(Fir.Type.Basic.bool ())
  in
  Q.Generator.union
    (List.filter_opt
       [ ( if has_int then
           Some
             (let module I = Int (Src) (Dst) in
             I.quickcheck_generator)
         else None )
       ; ( if has_bool then
           Some
             (let module B = Bool (Src) (Dst) in
             B.quickcheck_generator)
         else None ) ])
