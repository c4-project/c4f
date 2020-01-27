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
end

type t = {arguments: Expression.t list; function_id: Act_common.C_id.t}
[@@deriving make, fields, sexp, compare, equal]

module On_expressions :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Expression.t =
Travesty.Traversable.Make0 (struct
  type nonrec t = t

  module Elt = Expression

  module On_monad (M : Monad.S) = struct
    module Ls = Tx.List.On_monad (M)

    let map_m (x : t) ~(f : Expression.t -> Expression.t M.t) : t M.t =
      M.Let_syntax.(
        let%map arguments = Ls.map_m ~f x.arguments in
        make ~function_id:x.function_id ~arguments ())
  end
end)

module On_lvalues :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Lvalue.t =
  Travesty.Traversable.Chain0 (On_expressions) (Expression.On_lvalues)

module On_addresses :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Address.t =
  Travesty.Traversable.Chain0 (On_expressions) (Expression.On_addresses)
