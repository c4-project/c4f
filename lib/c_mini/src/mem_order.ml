(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the "Software"),
   to deal in the Software without restriction, including without limitation
   the rights to use, copy, modify, merge, publish, distribute, sublicense,
   and/or sell copies of the Software, and to permit persons to whom the
   Software is furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
   THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
   DEALINGS IN THE SOFTWARE. *)

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
