(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

module M = struct
  (* These are ordered in the same order given by
     https://en.cppreference.com/w/c/atomic/memory_order; this, in turn,
     seems to be a heuristic for the 'strength' of the memory order (later
     orders are stronger than earlier ones). *)
  type t = Relaxed | Consume | Acquire | Release | Acq_rel | Seq_cst
  [@@deriving enum]

  let table =
    [ (Relaxed, "memory_order_relaxed")
    ; (Consume, "memory_order_consume")
    ; (Acquire, "memory_order_acquire")
    ; (Release, "memory_order_release")
    ; (Acq_rel, "memory_order_acq_rel")
    ; (Seq_cst, "memory_order_seq_cst") ]
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

let can_change_dir (current : t) ~(replacement : t)
    ~(direction : [< `Strengthen | `Weaken | `Any]) : bool =
  match direction with
  | `Strengthen ->
      current <= replacement
  | `Weaken ->
      replacement <= current
  | `Any ->
      true

let can_change (current : t) ~(replacement : t) ~(is_compatible : t -> bool)
    ~(direction : [< `Strengthen | `Weaken | `Any]) : bool =
  is_compatible replacement && can_change_dir current ~replacement ~direction

let try_change (current : t) ~(replacement : t) ~(is_compatible : t -> bool)
    ~(direction : [< `Strengthen | `Weaken | `Any]) : t =
  if can_change current ~replacement ~is_compatible ~direction then
    replacement
  else current

let gen_load : t Base_quickcheck.Generator.t =
  Base_quickcheck.Generator.filter ~f:is_load_compatible
    [%quickcheck.generator: t]

let gen_store : t Base_quickcheck.Generator.t =
  Base_quickcheck.Generator.filter ~f:is_store_compatible
    [%quickcheck.generator: t]

let gen_rmw : t Base_quickcheck.Generator.t =
  Base_quickcheck.Generator.filter ~f:is_rmw_compatible
    [%quickcheck.generator: t]
