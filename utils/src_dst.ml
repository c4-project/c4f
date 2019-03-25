(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

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

open Core

type order =
  | Src_then_dst
  | Dst_then_src
[@@deriving sexp]

type ('s, 'd) t =
  { src : 's
  ; dst : 'd
  }
[@@deriving sexp, fields]

let get (sd : ('o, 'o) t) = function
  | `Src -> sd.src
  | `Dst -> sd.dst
;;

module type Has_order = sig
  val operand_order : order
end

module type S = sig
  include Has_order

  val of_src_dst : ('o, 'o) t -> 'o list
  val to_src_dst_or_error : 'o list -> ('o, 'o) t Or_error.t
  val to_src_dst : 'o list -> ('o, 'o) t option
  val bind_src_dst : f:(('o, 'o) t -> ('o, 'o) t option) -> 'o list -> 'o list option
  val map_src_dst : f:(('o, 'o) t -> ('o, 'o) t) -> 'o list -> 'o list option
end

(** [Make] takes a [Has_order] and generates the appropriate utility module. *)
module Make (H : Has_order) : S = struct
  include H

  let of_src_dst { src; dst } =
    match operand_order with
    | Src_then_dst -> [ src; dst ]
    | Dst_then_src -> [ dst; src ]
  ;;

  let to_src_dst_or_error = function
    | [ o1; o2 ] ->
      Ok
        (match operand_order with
        | Src_then_dst -> { src = o1; dst = o2 }
        | Dst_then_src -> { src = o2; dst = o1 })
    | xs ->
      Or_error.error_s [%message "Expected two operands" ~got:(List.length xs : int)]
  ;;

  let to_src_dst xs = Result.ok (to_src_dst_or_error xs)
  let bind_src_dst ~f xs = Option.(to_src_dst xs >>= f >>| of_src_dst)
  let map_src_dst ~f = bind_src_dst ~f:(fun sd -> Some (f sd))
end
