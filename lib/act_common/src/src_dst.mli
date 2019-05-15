(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to permit
   persons to whom the Software is furnished to do so, subject to the
   following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
   NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
   DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
   OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
   USE OR OTHER DEALINGS IN THE SOFTWARE. *)

(** Utilities for handling source-and-destination pairs

    Different assembly syntaxes order the source and destination of move and
    arithmetic operands in different ways. Infamously, x86 has two different
    syntaxes---AT&T used in GNU as, Intel used nearly everywhere else---that
    have opposite conventions!

    This module contains various types and module plumbing for handling
    these cases in a fairly unambiguous way. *)

open Core

(** [order] enumerates the different orders of operands for two-operand
    instructions. *)
type order =
  | Src_then_dst (* Source then destination (eg. in x86, AT&T) *)
  | Dst_then_src (* Destination then source (eg. in x86, Intel) *)
[@@deriving sexp]

(** [t] is a source and destination pair, with unambiguous names. *)
type ('s, 'd) t = {src: 's; dst: 'd} [@@deriving sexp]

val src : ('s, 'd) t -> 's
(** [src sd] gets the source of [sd]. *)

val dst : ('s, 'd) t -> 'd
(** [dst sd] gets the destination of [sd]. *)

val get : ('o, 'o) t -> [< `Src | `Dst] -> 'o
(** [get sd pos] gets the the source of [sd] if [pos] is [`Src], or the
    destination if [pos] is [`Dst]. The types of source and destination must
    be the same. *)

(** [Has_order] is a module signature for things that have a particular
    ordering for two-operand instructions. *)
module type Has_order = sig
  val operand_order : order
  (** [operand_order] gets the operand order of this module. *)
end

(** [S] is the signature of modules created with [Make], containing operand
    order conversion utilities. *)
module type S = sig
  include Has_order

  val of_src_dst : ('o, 'o) t -> 'o list
  (** [of_src_dst {src; dst}] converts [src] and [dst] into an operand list,
      with the arguments in the correct order for this dialect. Both
      operands must have the same type. *)

  val to_src_dst_or_error : 'o list -> ('o, 'o) t Or_error.t
  (** [to_src_dst_or_error ops] tries to interpret [ops] as a pair of source
      and destination operand. It returns an error if there are too few or
      too many operands. *)

  val to_src_dst : 'o list -> ('o, 'o) t option
  (** [to_src_dst ops] tries to interpret [ops] as a pair of source and
      destination operand. It differs from [to_src_dst_or_error] by
      returning [None] rather than a full error on failure. *)

  val bind_src_dst :
    f:(('o, 'o) t -> ('o, 'o) t option) -> 'o list -> 'o list option
  (** [bind_src_dst ~f ops] tries to interpret [ops] as a pair of source and
      destination operand, and binds [f] over them if successful. *)

  val map_src_dst :
    f:(('o, 'o) t -> ('o, 'o) t) -> 'o list -> 'o list option
  (** [maps_src_dst ~f ops] tries to interpret [ops] as a pair of source and
      destination operand, and maps [f] over them if successful. *)
end

(** [Make] takes a [Has_order] and generates the appropriate utility module. *)
module Make (H : Has_order) : S
