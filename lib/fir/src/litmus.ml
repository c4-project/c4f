(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module Tx = Travesty_base_exts
  module A = Accessor
  module Ac = Act_common
  module Named = Ac.C_named
end

module Lang :
  Act_litmus.Test_types.Basic
    with type Statement.t =
          [`Stm of unit Statement.t | `Decl of Initialiser.t Named.t]
     and type Program.t = unit Function.t Named.t
     and type Constant.t = Constant.t = struct
  module Constant = Constant

  module Statement = struct
    type t = [`Stm of unit Statement.t | `Decl of Initialiser.t Named.t]
    [@@deriving sexp]

    let empty () : t =
      `Stm A.(construct (Statement.prim' @> Prim_statement.nop) ())

    let make_uniform = Tx.List.right_pad ~padding:(empty ())
  end

  module Type = Type

  module Program = struct
    type t = unit Function.t Named.t [@@deriving sexp]

    let name (nd : t) : string option =
      Some (Ac.C_id.to_string (Named.name nd))

    let listing (np : t) : Statement.t list =
      let fn = Named.value np in
      List.map (Function.body_decls fn) ~f:(fun (name, value) ->
          `Decl (Named.make value ~name))
      @ List.map (Function.body_stms fn) ~f:(fun x -> `Stm x)

    let global_vars (np : t) =
      np |> Named.value |> Function.parameters
      |> Map.of_alist_or_error (module Ac.C_id)
      |> Result.ok
  end

  let name = "C"
end

module Test = Act_litmus.Test.Make (Lang)
