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

type 'meta t =
  { arguments: Expression.t list
  ; metadata: 'meta
  ; function_id: Act_common.C_id.t
  } [@@deriving make, fields, sexp, equal]

module On_meta : Travesty.Traversable_types.S1 with type 'meta t = 'meta t =
Travesty.Traversable.Make1 (struct
  type nonrec 'meta t = 'meta t

  module On_monad (M : Monad.S) = struct
    let map_m (x : 'm1 t) ~(f : 'm1 -> 'm2 M.t) : 'm2 t M.t =
      M.Let_syntax.(
        let%map metadata = f x.metadata in
        (* TODO(@MattWindsor91): expression metadata *)
        make ~metadata ~function_id:x.function_id ~arguments:x.arguments ()
      )
  end
end)

module With_meta (Meta : T) = struct
  module On_expressions:
    Travesty.Traversable_types.S0
      with type t = Meta.t t
       and type Elt.t = Expression.t = Travesty.Traversable.Make0 (struct
    type nonrec t = Meta.t t
    module Elt = Expression

    module On_monad (M : Monad.S) = struct
      module Ls = Tx.List.On_monad (M)
      let map_m (x : t) ~(f : Expression.t -> Expression.t M.t) : t M.t =
        M.Let_syntax.(
          let%map arguments = Ls.map_m ~f x.arguments in
          make ~metadata:x.metadata ~function_id:x.function_id ~arguments ()
        )
    end
  end)

  module On_lvalues :
    Travesty.Traversable_types.S0
      with type t = Meta.t t
       and type Elt.t = Lvalue.t =
    Travesty.Traversable.Chain0 (On_expressions) (Expression.On_lvalues)

  module On_addresses :
    Travesty.Traversable_types.S0
      with type t = Meta.t t
       and type Elt.t = Address.t =
    Travesty.Traversable.Chain0 (On_expressions) (Expression.On_addresses)

  module On_identifiers :
    Travesty.Traversable_types.S0
      with type t = Meta.t t
       and type Elt.t = Act_common.C_id.t =
    Travesty.Traversable.Chain0 (On_expressions) (Expression.On_identifiers)
end
