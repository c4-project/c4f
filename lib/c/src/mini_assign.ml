(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

type t = {lvalue: Mini_lvalue.t; rvalue: Mini_expression.t}
[@@deriving sexp, fields, make]

module Base_map (M : Monad.S) = struct
  module F = Travesty.Traversable.Helpers (M)

  let bmap (assign : t) ~(lvalue : Mini_lvalue.t F.traversal)
      ~(rvalue : Mini_expression.t F.traversal) : t M.t =
    Fields.fold ~init:(M.return assign) ~lvalue:(F.proc_field lvalue)
      ~rvalue:(F.proc_field rvalue)
end

module On_lvalues :
  Travesty.Traversable_types.S0
    with type t = t
     and type Elt.t = Mini_lvalue.t = Travesty.Traversable.Make0 (struct
  type nonrec t = t

  module Elt = Mini_lvalue

  module On_monad (M : Monad.S) = struct
    module B = Base_map (M)
    module E = Mini_expression.On_lvalues.On_monad (M)

    let map_m x ~f = B.bmap x ~lvalue:f ~rvalue:(E.map_m ~f)
  end
end)

module On_addresses :
  Travesty.Traversable_types.S0
    with type t = t
     and type Elt.t = Mini_address.t = Travesty.Traversable.Make0 (struct
  type nonrec t = t

  module Elt = Mini_address

  module On_monad (M : Monad.S) = struct
    module B = Base_map (M)
    module E = Mini_expression.On_addresses.On_monad (M)

    let map_m x ~f = B.bmap x ~lvalue:M.return ~rvalue:(E.map_m ~f)
  end
end)
