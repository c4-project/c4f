(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation
   files (the "Software"), to deal in the Software without
   restriction, including without limitation the rights to use, copy,
   modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

(** Enumeration of C11 memory orders. *)

open Core_kernel

type t =
  | Seq_cst (** [memory_order_seq_cst] *)
  | Release (** [memory_order_release] *)
  | Acquire (** [memory_order_acquire] *)
  | Acq_rel (** [memory_order_acq_rel] *)
  | Relaxed (** [memory_order_relaxed] *)
  | Consume (** [memory_order_consume] *)

include Utils.Enum.S_table with type t := t
include Utils.Enum.Extension_table with type t := t

(** {2 Predicates} *)

(** [is_load_compatible mo] returns whether [mo] is compatible with
   load operations. *)
val is_load_compatible : t -> bool

(** [is_store_compatible mo] returns whether [mo] is compatible with
    store operations. *)
val is_store_compatible : t -> bool

(** [is_rmw_compatible mo] returns whether [mo] is compatible with
    read-modify-writes. *)
val is_rmw_compatible : t -> bool

(** {2 Quickcheck support} *)

(** The Quickcheck implementation pulled in by [Extension_table], by
   default, generates from the whole pool of memory orders.  To
   restrict to those that make sense in a particular context, see
   {{!gen_load}} et al. *)

(** [gen_load] generates a random memory order suitable for loads. *)
val gen_load : t Quickcheck.Generator.t

(** [gen_load] generates a random memory order suitable for stores. *)
val gen_store : t Quickcheck.Generator.t

(** [gen_load] generates a random memory order suitable for
   read-modify-writes. *)
val gen_rmw : t Quickcheck.Generator.t
