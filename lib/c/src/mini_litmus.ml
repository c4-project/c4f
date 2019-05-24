(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to permit
   persons to whom the Software is furnished to do so, subject to the
   following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
   NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
   DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
   OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
   USE OR OTHER DEALINGS IN THE SOFTWARE. *)

open Core_kernel
module Tx = Travesty_core_kernel_exts
open Mini
module Ac = Act_common

module Lang :
  Act_litmus.Ast.Basic
  with type Statement.t =
              [`Stm of Statement.t | `Decl of Initialiser.t named]
   and type Program.t = Function.t named
   and type Constant.t = Constant.t = struct
  module Constant = Constant

  module Statement = struct
    type t = [`Stm of Statement.t | `Decl of Initialiser.t named]
    [@@deriving sexp]

    let reify = function
      | `Decl (id, init) ->
          `Decl (Mini_reify.decl id init)
      | `Stm stm ->
          `Stm (Mini_reify.stm stm)

    let pp = Fmt.using reify Ast.Litmus_lang.Statement.pp

    let empty () = `Stm (Statement.nop ())

    let make_uniform = Tx.List.right_pad ~padding:(empty ())
  end

  module Type = Type

  module Program = struct
    type t = Function.t named [@@deriving sexp]

    let name (n, _) = Some (Ac.C_id.to_string n)

    let listing (_, fn) =
      List.map (Function.body_decls fn) ~f:(fun x -> `Decl x)
      @ List.map (Function.body_stms fn) ~f:(fun x -> `Stm x)

    let pp =
      Fmt.(using (Tuple2.uncurry Mini_reify.func) Ast.External_decl.pp)

    let global_vars (_, fn) =
      fn |> Function.parameters |> Ac.C_id.Map.of_alist_or_error
      |> Result.ok
  end

  let name = "C"
end

module Ast = Act_litmus.Ast.Make (Lang)
module Pp = Act_litmus.Pp.Make_sequential (Ast)

let function_cvars_map (tid : int) ((_, func) : Lang.Program.t) :
    Ac.C_variables.Map.t =
  func |> Function.cvars
  |> Ac.C_variables.Map.of_single_scope_set ~tid
       ~scope:Ac.C_variables.Scope.Local

let litmus_local_cvars (ast : Ast.Validated.t) : Ac.C_variables.Map.t list =
  ast |> Ast.Validated.programs |> List.mapi ~f:function_cvars_map

let constant_to_initial_value : Constant.t -> Ac.C_variables.Initial_value.t
    = function
  | Integer k ->
      Some k
  | Char _ | Float _ ->
      None

(* for now *)

let litmus_global_cvars (ast : Ast.Validated.t) : Ac.C_variables.Map.t =
  ast |> Ast.Validated.init
  |> List.map ~f:(fun (var, k) -> (var, constant_to_initial_value k))
  |> Ac.C_id.Map.of_alist_exn (* for now *)
  |> Ac.C_variables.Map.of_single_scope_map ~scope:Global

let cvars (ast : Ast.Validated.t) : Ac.C_variables.Map.t =
  let locals = litmus_local_cvars ast in
  let globals = litmus_global_cvars ast in
  Ac.C_variables.Map.merge_list (globals :: locals)
