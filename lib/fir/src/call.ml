(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module Tx = Travesty_base_exts
end

type t = {arguments: Expression.t list; function_id: C4f_common.C_id.t}
[@@deriving make, fields, sexp, compare, equal]

module On_expressions :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Expression.t =
Travesty.Traversable.Make0 (struct
  type nonrec t = t

  module Elt = Expression

  module On (M : Applicative.S) = struct
    module Ls = Tx.List.On (M)

    let map_m (x : t) ~(f : Expression.t -> Expression.t M.t) : t M.t =
      M.map (Ls.map_m ~f x.arguments) ~f:(fun arguments ->
          make ~function_id:x.function_id ~arguments ())
  end
end)

module On_addresses :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Address.t =
  Travesty.Traversable.Chain0
    (On_expressions)
    (Expression_traverse.On_addresses)
