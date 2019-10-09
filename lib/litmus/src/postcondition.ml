(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Id = Act_common.Litmus_id
module Tx = Travesty_base_exts

module Quantifier = struct
  module M = struct
    type t = Exists | For_all [@@deriving enum, quickcheck]

    let table : (t, string) List.Assoc.t =
      [(Exists, "exists"); (For_all, "forall")]
  end

  include M
  include Act_utils.Enum.Extend_table (M)
end

type 'const t = {quantifier: Quantifier.t; predicate: 'const Predicate.t}
[@@deriving sexp, compare, equal, quickcheck, fields, make]

module BT :
  Travesty.Bi_traversable_types.S1_right
    with type 'const t := 'const t
     and type left = Id.t = Travesty.Bi_traversable.Make1_right (struct
  type nonrec 'const t = 'const t

  type left = Id.t

  module On_monad (M : Monad.S) = struct
    module Pr = Predicate.On_monad (M)

    let bi_map_m (t : 'a t) ~(left : Id.t -> Id.t M.t)
        ~(right : 'a -> 'b M.t) : 'b t M.t =
      let quantifier = quantifier t in
      M.Let_syntax.(
        let%map predicate = Pr.bi_map_m ~left ~right (predicate t) in
        make ~quantifier ~predicate)
  end
end)

include BT

module On_c_identifiers :
  Travesty.Bi_traversable_types.S1_right
    with type 'const t = 'const t
     and type left = Act_common.C_id.t =
Travesty.Bi_traversable.Make1_right (struct
  type nonrec 'const t = 'const t

  type left = Act_common.C_id.t

  module On_monad (M : Monad.S) = struct
    module Lid_cid = Act_common.Litmus_id.On_c_identifiers.On_monad (M)
    module B = On_monad (M)

    let bi_map_m (t : 'a t)
        ~(left : Act_common.C_id.t -> Act_common.C_id.t M.t)
        ~(right : 'a -> 'b M.t) : 'b t M.t =
      B.bi_map_m ~left:(Lid_cid.map_m ~f:left) ~right t
  end
end)

let pp (f : Formatter.t) (pc : 'const t) ~(pp_const : 'const Fmt.t) : unit =
  let predicate = predicate pc in
  let quantifier = quantifier pc in
  Fmt.(box (pair ~sep:sp Quantifier.pp (parens (Predicate.pp ~pp_const))))
    f (quantifier, predicate)
