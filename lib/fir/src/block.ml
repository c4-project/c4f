(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

type ('meta, 'stm) t = {statements: 'stm list; metadata: 'meta}
[@@deriving sexp, compare, equal, accessors, make]

let of_statement_list (type stm) (statements : stm list) : (unit, stm) t =
  make ~statements ~metadata:() ()

let is_empty (type meta stm) (block : (meta, stm) t) : bool =
  List.is_empty block.statements

let each_statement : ('i, 'stm, ('meta, 'stm) t, [< many]) Accessor.t =
  [%accessor statements @> Accessor.List.each]

module BT :
  Travesty.Bi_traversable_types.S2
    with type ('meta, 'stm) t = ('meta, 'stm) t =
Travesty.Bi_traversable.Make2 (struct
  type nonrec ('meta, 'stm) t = ('meta, 'stm) t

  module On (M : Applicative.S) = struct
    module AccM = Accessor.Of_applicative (M)

    let bi_map_m (type m1 m2 s1 s2) (block : (m1, s1) t)
        ~(left : m1 -> m2 M.t) ~(right : s1 -> s2 M.t) : (m2, s2) t M.t =
      M.map2
        ~f:(fun metadata statements -> make ~metadata ~statements ())
        (left block.metadata)
        (AccM.map Accessor.List.each ~f:right block.statements)
  end
end)

include (BT : module type of BT with type ('meta, 'stm) t := ('meta, 'stm) t)

module On_statements (Meta : T) :
  Travesty.Traversable_types.S1 with type 'stm t := (Meta.t, 'stm) t =
  Travesty.Bi_traversable.Traverse1_right
    (Travesty.Bi_traversable.Fix2_left (BT) (Meta))

module On_meta_statement_list (Stm : T1) = struct
  module On (M : Applicative.S) = struct
    module AccM = Accessor.Of_applicative (M)

    let map_m (type meta) (block : (meta, meta Stm.t) t)
        ~(f : meta Stm.t list -> meta Stm.t list M.t) :
        (meta, meta Stm.t) t M.t =
      AccM.map statements block ~f
  end
end
