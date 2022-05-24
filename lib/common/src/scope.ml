(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

module M = struct
  type t = Local of int | Global
  [@@deriving accessors, compare, equal, sexp]
end

include M
include Comparable.Make (M)

let is_global : t -> bool = function Local _ -> false | Global -> true

let reduce (type a) (l : t * a) (r : t * a) : t * a =
  if Int.(Comparable.lift ~f:fst [%compare: M.t] l r <= 0) then l else r

let pp (f : Formatter.t) (x : t) : unit =
  match x with Local i -> Fmt.pf f "P%d" i | Global -> Fmt.pf f "global"
