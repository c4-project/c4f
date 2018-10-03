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

module type S = sig
  type t
  type cont

  val fold_map : f:('a -> t -> 'a * t) -> init:'a -> cont -> ('a * cont)
end

module type Intf = sig
  include S

  val map : f:(t -> t) -> cont -> cont
  val fold : f:('a -> t -> 'a) -> init:'a -> cont -> 'a
  val list : cont -> t list
  val for_all : f:(t -> bool) -> cont -> bool
  val exists : f:(t -> bool) -> cont -> bool
end

module Make (I : S) = struct
  include I

  let map ~f c = snd (fold_map ~f:(fun () x -> ((), f x)) ~init:() c)

  let fold ~f ~init c =
    fst (fold_map ~f:(fun acc x -> f acc x, x) ~init c)

  let list c = fold ~f:(Fn.flip List.cons) ~init:[] c

  let for_all ~f c = List.for_all ~f (list c)

  let exists ~f c = List.exists ~f (list c)
end

module type SetIntf = sig
  include Intf

  module Set : Core.Set.S with type Elt.t = t

  val set : cont -> Set.t
end

module MakeSet (I : S) (Set : Core.Set.S with type Elt.t = I.t) = struct
  include Make (I)

  module Set = Set

  let set c =
    fst (fold_map ~f:(fun set x -> Set.add set x, x) ~init:Set.empty c)
end
