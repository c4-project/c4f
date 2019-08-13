(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

type order = Src_then_dst | Dst_then_src [@@deriving sexp]

type ('s, 'd) t = {src: 's; dst: 'd} [@@deriving sexp, fields]

let get (sd : ('o, 'o) t) = function `Src -> sd.src | `Dst -> sd.dst

module type Has_order = sig
  val operand_order : order
end

module type S = sig
  include Has_order

  val of_src_dst : ('o, 'o) t -> 'o list

  val to_src_dst_or_error : 'o list -> ('o, 'o) t Or_error.t

  val to_src_dst : 'o list -> ('o, 'o) t option

  val bind_src_dst :
    f:(('o, 'o) t -> ('o, 'o) t option) -> 'o list -> 'o list option

  val map_src_dst :
    f:(('o, 'o) t -> ('o, 'o) t) -> 'o list -> 'o list option
end

(** [Make] takes a [Has_order] and generates the appropriate utility module. *)
module Make (H : Has_order) : S = struct
  include H

  let of_src_dst {src; dst} =
    match operand_order with
    | Src_then_dst ->
        [src; dst]
    | Dst_then_src ->
        [dst; src]

  let to_src_dst_or_error = function
    | [o1; o2] ->
        Ok
          ( match operand_order with
          | Src_then_dst ->
              {src= o1; dst= o2}
          | Dst_then_src ->
              {src= o2; dst= o1} )
    | xs ->
        Or_error.error_s
          [%message "Expected two operands" ~got:(List.length xs : int)]

  let to_src_dst xs = Result.ok (to_src_dst_or_error xs)

  let bind_src_dst ~f xs = Option.(to_src_dst xs >>= f >>| of_src_dst)

  let map_src_dst ~f = bind_src_dst ~f:(fun sd -> Some (f sd))
end
