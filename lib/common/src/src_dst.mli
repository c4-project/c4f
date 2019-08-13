(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Utilities for handling source-and-destination pairs

    Different assembly syntaxes order the source and destination of move and
    arithmetic operands in different ways. Infamously, x86 has two different
    syntaxes---AT&T used in GNU as, Intel used nearly everywhere else---that
    have opposite conventions!

    This module contains various types and module plumbing for handling
    these cases in a fairly unambiguous way. *)

open Base

(** [order] enumerates the different orders of operands for two-operand
    instructions. *)
type order =
  | Src_then_dst (* Source then destination (eg. in x86, AT&T) *)
  | Dst_then_src (* Destination then source (eg. in x86, Intel) *)
[@@deriving sexp]

type ('s, 'd) t = {src: 's; dst: 'd} [@@deriving sexp]
(** [t] is a source and destination pair, with unambiguous names. *)

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

module Make (H : Has_order) : S
(** [Make] takes a [Has_order] and generates the appropriate utility module. *)
