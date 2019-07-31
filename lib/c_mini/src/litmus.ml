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

open Base
module Tx = Travesty_base_exts
module Ac = Act_common

module Lang :
  Act_litmus.Ast_types.Basic
    with type Statement.t =
          [`Stm of Statement.t | `Decl of Initialiser.t Named.t]
     and type Program.t = Function.t Named.t
     and type Constant.t = Constant.t = struct
  module Constant = Constant

  module Statement = struct
    type t = [`Stm of Statement.t | `Decl of Initialiser.t Named.t]
    [@@deriving sexp]

    let reify : t -> _ = function
      | `Decl named_init ->
          `Decl
            (Reify.decl (Named.name named_init) (Named.value named_init))
      | `Stm stm ->
          `Stm (Reify.stm stm)

    let pp : t Fmt.t =
      Fmt.using reify Act_c_lang.Ast.Litmus_lang.Statement.pp

    let empty () : t = `Stm (Statement.nop ())

    let make_uniform = Tx.List.right_pad ~padding:(empty ())
  end

  module Type = Type

  module Program = struct
    type t = Function.t Named.t [@@deriving sexp]

    let name (nd : t) : string option =
      Some (Ac.C_id.to_string (Named.name nd))

    let listing (np : t) : Statement.t list =
      let fn = Named.value np in
      List.map (Function.body_decls fn) ~f:(fun (name, value) ->
          `Decl (Named.make value ~name))
      @ List.map (Function.body_stms fn) ~f:(fun x -> `Stm x)

    let pp : t Fmt.t =
      Fmt.(
        using
          (fun np -> Reify.func (Named.name np) (Named.value np))
          Act_c_lang.Ast.External_decl.pp)

    let global_vars (np : t) =
      np |> Named.value |> Function.parameters
      |> Ac.C_id.Map.of_alist_or_error |> Result.ok
  end

  let name = "C"
end

module Ast = Act_litmus.Ast.Make (Lang)
module Pp = Act_litmus.Pp.Make_sequential (Ast)
