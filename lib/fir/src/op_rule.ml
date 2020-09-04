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

(** Type of outcomes of algebraic questions such as 'does [x op x] return
    a particular statically-known value?'. *)
type t =
  | Idem
  | Zero
  | Unknown
  [@@deriving accessors]

let is_present (acc : ('a, unit, t, [< A.getter]) A.Simple.t) (x : t) : bool =
  not (A.is_empty acc x)

let is_idem : t -> bool = is_present idem

let is_zero : t -> bool = is_present zero