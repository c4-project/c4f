(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Tx = Travesty_base_exts

type ('meta, 'stm) t = {statements: 'stm list; metadata: 'meta}
[@@deriving sexp, equal, fields, make]

let of_statement_list (type stm) (statements : stm list) : (unit, stm) t =
  make ~statements ~metadata:() ()

let is_empty (type meta stm) (block : (meta, stm) t) : bool =
  List.is_empty block.statements

module BT :
  Travesty.Bi_traversable_types.S2
    with type ('meta, 'stm) t = ('meta, 'stm) t =
Travesty.Bi_traversable.Make2 (struct
  type nonrec ('meta, 'stm) t = ('meta, 'stm) t

  module On_monad (M : Monad.S) = struct
    module L = Tx.List.On_monad (M)

    let bi_map_m (type m1 m2 s1 s2) (block : (m1, s1) t)
        ~(left : m1 -> m2 M.t) ~(right : s1 -> s2 M.t) : (m2, s2) t M.t =
      M.Let_syntax.(
        let%map metadata' = left (metadata block)
        and statements' = L.map_m ~f:right (statements block) in
        make ~metadata:metadata' ~statements:statements' ())
  end
end)

include (BT : module type of BT with type ('meta, 'stm) t := ('meta, 'stm) t)

module On_statements (Meta : T) :
  Travesty.Traversable_types.S1 with type 'stm t := (Meta.t, 'stm) t =
  Travesty.Bi_traversable.Traverse1_right
    (Travesty.Bi_traversable.Fix2_left (BT) (Meta))

module On_meta_statement_list (Stm : T1) = struct
  module On_monad (M : Monad.S) = struct
    let map_m (type meta) (block : (meta, meta Stm.t) t)
        ~(f : meta Stm.t list -> meta Stm.t list M.t) :
        (meta, meta Stm.t) t M.t =
      M.Let_syntax.(
        let%map statements' = f (statements block) in
        make ~metadata:(metadata block) ~statements:statements' ())
  end
end
