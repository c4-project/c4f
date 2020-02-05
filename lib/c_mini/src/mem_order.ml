(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

module M = struct
  type t = Seq_cst | Release | Acquire | Acq_rel | Relaxed | Consume
  [@@deriving enum]

  let table =
    [ (Seq_cst, "memory_order_seq_cst")
    ; (Release, "memory_order_release")
    ; (Acquire, "memory_order_acquire")
    ; (Acq_rel, "memory_order_acq_rel")
    ; (Relaxed, "memory_order_relaxed")
    ; (Consume, "memory_order_consume") ]
end

include M
include Act_utils.Enum.Extend_table (M)

(* Make doubly sure that we don't degrade memory orders to invalid
   equivalents. *)
let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic

let is_load_compatible : t -> bool = function
  | Seq_cst | Acquire | Consume | Relaxed ->
      true
  | Release | Acq_rel ->
      false

let is_store_compatible : t -> bool = function
  | Seq_cst | Release | Relaxed ->
      true
  | Acquire | Consume | Acq_rel ->
      false

let is_rmw_compatible : t -> bool = function
  | Seq_cst | Acq_rel | Relaxed ->
      true
  | Acquire | Consume | Release ->
      false

let gen_load : t Base_quickcheck.Generator.t =
  Base_quickcheck.Generator.filter ~f:is_load_compatible
    [%quickcheck.generator: t]

let gen_store : t Base_quickcheck.Generator.t =
  Base_quickcheck.Generator.filter ~f:is_store_compatible
    [%quickcheck.generator: t]

let gen_rmw : t Base_quickcheck.Generator.t =
  Base_quickcheck.Generator.filter ~f:is_rmw_compatible
    [%quickcheck.generator: t]
