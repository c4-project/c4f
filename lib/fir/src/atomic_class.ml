(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

let const_some (x : 'a) (_ : 'b) : 'a option = Some x

module M = struct
  type t = Cmpxchg | Fence | Fetch | Load | Store [@@deriving enum]

  let table : (t, string) List.Assoc.t =
    [ (Cmpxchg, "cmpxchg")
    ; (Fence, "fence")
    ; (Fetch, "fetch")
    ; (Load, "load")
    ; (Store, "store") ]
end

include M
include C4f_utils.Enum.Extend_table (M)

let classify_stm : Atomic_statement.t -> t option =
  Atomic_statement.value_map ~cmpxchg:(const_some Cmpxchg)
    ~fence:(const_some Fence) ~fetch:(const_some Fetch)
    ~store:(const_some Store)

let classify_expr (ex : _ Atomic_expression.t) : t option =
  Atomic_expression.reduce ex ~cmpxchg:(const_some Cmpxchg)
    ~fetch:(const_some Fetch) ~load:(const_some Load)

let matches (clazz : t) ~(template : t) : bool = equal clazz template
