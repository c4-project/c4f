(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

module Post_op = struct
  module M = struct
    type t =
      | Add
      | Sub
    [@@deriving enum]

    (* TODO(@MattWindsor91): or, xor, and *)

    let table =
      [ (Add, "add")
      ; (Sub, "sub")
      ]
  end

  include M
  include Act_utils.Enum.Extend_table (M)
end

type 'e t =
  { obj: Address.t
  ; arg: 'e
  ; mo: Mem_order.t
  ; op: Post_op.t
  }
[@@deriving sexp, fields, make, compare, equal]
