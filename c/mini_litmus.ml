(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation
   files (the "Software"), to deal in the Software without
   restriction, including without limitation the rights to use, copy,
   modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

open Core_kernel
open Utils

open Mini

module Lang : Litmus.Ast.Basic
  with type Statement.t = [`Stm of Statement.t | `Decl of Initialiser.t named]
   and type Program.t = Function.t named
   and type Constant.t = Constant.t = (struct
    module Constant = Constant

    module Statement = struct
      type t = [`Stm of Statement.t | `Decl of Initialiser.t named]
      [@@deriving sexp]

      let reify = function
        | `Decl (id, init) -> `Decl (Reify.decl id init)
        | `Stm stm         -> `Stm  (Reify.stm stm)
      ;;

      let pp = Fmt.using reify Ast.Litmus_lang.Statement.pp

      let empty () = `Stm (Statement.nop)
      let make_uniform = Travesty.T_list.right_pad ~padding:(empty ())
    end

    module Type = Type

    module Program = struct
      type t = Function.t named [@@deriving sexp]
      let name (n, _) = Some (C_identifier.to_string n)
      let listing (_, fn) =
        List.map (Function.body_decls fn) ~f:(fun x -> `Decl x)
        @ List.map (Function.body_stms fn) ~f:(fun x -> `Stm x)
      let pp = Fmt.(using (Tuple2.uncurry Reify.func) Ast.External_decl.pp)

      let global_vars (_, fn) =
        fn
        |> Function.parameters
        |> C_identifier.Map.of_alist_or_error
        |> Result.ok
      ;;
    end

    let name = "C"
  end)

module Ast = Litmus.Ast.Make (Lang)
module Pp = Litmus.Pp.Make_sequential (Ast)

let litmus_local_cvars (ast : Ast.Validated.t) : C_identifier.Set.t =
  ast
  |> Ast.Validated.programs
  |> List.map ~f:(fun (_, func) -> Function.cvars func)
  |> C_identifier.Set.union_list
;;

let litmus_global_cvars (ast : Ast.Validated.t) : C_identifier.Set.t =
  ast
  |> Ast.Validated.init
  |> List.map ~f:(fun (var, _) -> var)
  |> C_identifier.Set.of_list
;;

let cvars (ast : Ast.Validated.t) : C_identifier.Set.t =
  let locals = litmus_local_cvars ast in
  let globals = litmus_global_cvars ast in
  C_identifier.Set.union locals globals
;;
