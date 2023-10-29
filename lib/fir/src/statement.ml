(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

(* This module brought to you by ppx_deriving's dislike of nonrec. *)
module Inner = struct
  type 'meta t =
    | Prim of ('meta, Prim_statement.t) With_meta.t
    | If_stm of ('meta, 'meta t) If.t
    | Flow of ('meta, 'meta t) Flow_block.t
  [@@deriving sexp, compare, accessors, equal]
end

include Inner

let prim' : ('a, Prim_statement.t, unit t, [< variant]) Accessor.t =
  [%accessor Accessor.(prim @> With_meta.no_meta)]

let reduce_step (type meta result) (x : meta t)
    ~(prim : (meta, Prim_statement.t) With_meta.t -> result)
    ~(if_stm : (meta, meta t) If.t -> result)
    ~(flow : (meta, meta t) Flow_block.t -> result) : result =
  match x with Prim x -> prim x | If_stm x -> if_stm x | Flow x -> flow x

let reduce (type meta result) (x : meta t)
    ~(prim : (meta, Prim_statement.t) With_meta.t -> result)
    ~(if_stm : (meta, result) If.t -> result)
    ~(flow : (meta, result) Flow_block.t -> result) : result =
  let rec mu x =
    let block_map (x : (meta, meta t) Block.t) : (meta, result) Block.t =
      Block.map_right x ~f:mu
    in
    Tx.Fn.Compose_syntax.(
      reduce_step x ~prim
        ~if_stm:
          ( If.map ~cond:Fn.id ~t_branch:block_map ~f_branch:block_map
          >> if_stm )
        ~flow:(Flow_block.map ~body:block_map ~header:Fn.id >> flow) )
  in
  mu x

let own_metadata (type meta) (x : meta t) : meta Option.t =
  reduce_step x
    ~prim:(Accessor.get_option With_meta.meta)
    ~if_stm:(Fn.const None) ~flow:(Fn.const None)

(** Shorthand for lifting a predicate on primitives. *)
let is_prim_and (type meta) (t : meta t) ~(f : Prim_statement.t -> bool) :
    bool =
  Accessor.(exists (prim @> With_meta.value)) ~f t

(** Shorthand for writing a predicate that is [false] on primitives. *)
let is_not_prim_and (type meta) ~(if_stm : (meta, meta t) If.t -> bool)
    ~(flow : (meta, meta t) Flow_block.t -> bool) : meta t -> bool =
  reduce_step ~if_stm ~flow ~prim:(Fn.const false)

let true_of_any_block_stm (b : ('meta, 'meta t) Block.t)
    ~(predicate : 'meta t -> bool) : bool =
  Accessor.exists Block.each_statement b ~f:predicate

let true_of_any_flow_body_stm ({body; _} : ('meta, 'meta t) Flow_block.t)
    ~(predicate : 'meta t -> bool) : bool =
  true_of_any_block_stm body ~predicate

let is_if_statement (m : 'meta t) : bool =
  is_not_prim_and m ~if_stm:(Fn.const true) ~flow:(Fn.const false)

let rec has_if_statements (m : 'meta t) : bool =
  is_not_prim_and m ~if_stm:(Fn.const true)
    ~flow:(true_of_any_flow_body_stm ~predicate:has_if_statements)

let has_blocks_with_metadata (m : 'meta t) ~(predicate : 'meta -> bool) :
    bool =
  let rec mu x =
    let true_of_block b =
      predicate b.@(Block.metadata) || true_of_any_block_stm ~predicate:mu b
    in
    is_not_prim_and x
      ~flow:(fun {body; _} -> true_of_block body)
      ~if_stm:(fun {t_branch; f_branch; _} ->
        true_of_block t_branch || true_of_block f_branch )
  in
  mu m

module If = struct
  type 'meta t = ('meta, 'meta Inner.t) If.t
  [@@deriving sexp, compare, equal]
end

module Flow_block = struct
  type 'meta t = ('meta, 'meta Inner.t) Flow_block.t
  [@@deriving sexp, compare, equal]
end
