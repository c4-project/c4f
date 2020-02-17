(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

type t = {lvalue: Lvalue.t; rvalue: Expression.t}
[@@deriving sexp, fields, make, compare, equal]

module Base_map (M : Monad.S) = struct
  module F = Travesty.Traversable.Helpers (M)

  let bmap (assign : t) ~(lvalue : Lvalue.t F.traversal)
      ~(rvalue : Expression.t F.traversal) : t M.t =
    Fields.fold ~init:(M.return assign) ~lvalue:(F.proc_field lvalue)
      ~rvalue:(F.proc_field rvalue)
end

module On_lvalues :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Lvalue.t =
Travesty.Traversable.Make0 (struct
  type nonrec t = t

  module Elt = Lvalue

  module On_monad (M : Monad.S) = struct
    module B = Base_map (M)
    module E = Expression.On_lvalues.On_monad (M)

    let map_m x ~f = B.bmap x ~lvalue:f ~rvalue:(E.map_m ~f)
  end
end)

module On_expressions :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Expression.t =
Travesty.Traversable.Make0 (struct
  type nonrec t = t

  module Elt = Expression

  module On_monad (M : Monad.S) = struct
    module B = Base_map (M)

    let map_m x ~f = B.bmap x ~lvalue:M.return ~rvalue:f
  end
end)

module On_addresses :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Address.t =
  Travesty.Traversable.Chain0 (On_expressions) (Expression.On_addresses)
