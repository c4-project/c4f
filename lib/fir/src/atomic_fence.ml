(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

module Mode = struct
  module M = struct
    type t = Thread | Signal [@@deriving enum]

    let table = [(Thread, "thread"); (Signal, "signal")]
  end

  include M
  include C4f_utils.Enum.Extend_table (M)
end

type t = {mode: Mode.t; mo: Mem_order.t}
[@@deriving sexp, compare, equal, quickcheck, fields, make]

module On_mem_orders :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Mem_order.t =
Travesty.Traversable.Make0 (struct
  type nonrec t = t

  module Elt = Mem_order

  module On (M : Applicative.S) = struct
    let map_m (x : t) ~(f : Elt.t -> Elt.t M.t) : t M.t =
      M.(x.mo |> f >>| fun mo -> {x with mo})
  end
end)
