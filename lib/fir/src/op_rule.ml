(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open struct
  module A = Accessor_base
end

type t =
  | Idem
  | Const of Constant.t 
  | Unknown
  [@@deriving accessors]

let true_ : ('a, unit, t, [< A.variant]) A.Simple.t =
  [%accessor A.(const @> Constant.Acc.true_)]

let false_ : ('a, unit, t, [< A.variant]) A.Simple.t =
  [%accessor A.(const @> Constant.Acc.false_)]

let zero : ('a, unit, t, [< A.variant]) A.Simple.t =
  [%accessor A.(const @> Constant.Acc.zero)]

let mk_idem : unit -> t = A.construct idem
let mk_const : Constant.t -> t = A.construct const
let mk_true : unit -> t = A.construct true_
let mk_false : unit -> t = A.construct false_
let mk_zero : unit -> t = A.construct zero
let mk_unknown : unit -> t = A.construct unknown

let is_present (acc : (unit -> 'a -> 'b, unit -> 'at -> 'bt, [> A.many_getter]) A.t) (x : 'at) : bool =
  not (A.is_empty acc x)

let is_idem : t -> bool = is_present idem

let is_true : t -> bool = is_present true_
let is_false : t -> bool = is_present false_
let is_zero : t -> bool = is_present zero

