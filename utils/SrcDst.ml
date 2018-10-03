(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining
   a copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to
   permit persons to whom the Software is furnished to do so, subject to
   the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
   LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
   OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
   WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. *)

open Core

type order =
  | SrcDst
  | DstSrc

type ('s, 'd) t =
  { src : 's
  ; dst : 'd
  }

module type HasOrder =
sig
  val operand_order : order
end

module type S =
sig
  include HasOrder

  val of_src_dst : ('o, 'o) t -> 'o list
  val to_src_dst : 'o list -> ('o, 'o) t option
  val bind_src_dst : f:(('o, 'o) t -> ('o, 'o) t option) -> 'o list -> ('o list) option
  val map_src_dst : f:(('o, 'o) t -> ('o, 'o) t) -> 'o list -> ('o list) option
end

(** [Make] takes a [HasOrder] and generates the appropriate utility module. *)
module Make (H : HasOrder) =
struct
  include H

  let of_src_dst {src; dst} =
    match operand_order with
    | SrcDst -> [src; dst]
    | DstSrc -> [dst; src]

  let to_src_dst =
    function
    | [o1; o2] ->
      (match operand_order with
       | SrcDst -> Some {src = o1; dst = o2}
       | DstSrc -> Some {src = o2; dst = o1})
    | _ -> None

  let bind_src_dst ~f xs =
    let open Option in
    to_src_dst xs >>= f >>| of_src_dst

  let map_src_dst ~f = bind_src_dst ~f:(fun sd -> Some (f sd))
end
