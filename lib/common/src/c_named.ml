(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Accessor.O

(* This nested module used to fix shadowing later. *)
module M = struct
  type 'a t = {name: C_id.t; value: 'a}
  [@@deriving sexp, compare, equal, accessors, quickcheck]
end

include M

let make (value : 'a) ~(name : C_id.t) : 'a t = {name; value}

let tuple : ('i, C_id.t * 'a, 'a t, [< isomorphism]) Accessor.Simple.t =
  [%accessor
    Accessor.isomorphism
      ~construct:(fun (name, value) -> {name; value})
      ~get:(fun {name; value} -> (name, value))]

let seq_of_alist (xs : (C_id.t, 'a) List.Assoc.t) : 'a t Sequence.t =
  xs |> Sequence.of_list |> Sequence.map ~f:(Accessor.construct tuple)

let list_of_alist (xs : (C_id.t, 'a) List.Assoc.t) : 'a t list =
  xs |> seq_of_alist |> Sequence.to_list

let alist_of_seq (xs : 'a t Sequence.t) : (C_id.t, 'a) List.Assoc.t =
  xs |> Sequence.map ~f:(Accessor.get tuple) |> Sequence.to_list

let alist_of_list (xs : 'a t list) : (C_id.t, 'a) List.Assoc.t =
  xs |> Sequence.of_list |> alist_of_seq

module BT :
  Travesty.Bi_traversable_types.S1_right
    with type 'r t := 'r t
     and type left = C_id.t = Travesty.Bi_traversable.Make1_right (struct
  type nonrec 'r t = 'r t

  type left = C_id.t

  module On (M : Applicative.S) = struct
    let bi_map_m (n : 'r1 t) ~(left : left -> left M.t)
        ~(right : 'r1 -> 'r2 M.t) : 'r2 t M.t =
      M.map2 ~f:(fun name -> make ~name) (left n.@(name)) (right n.@(value))
  end
end)

include BT

module Alist = struct
  type 'a t = ((C_id.t * 'a) list[@sexp.list]) [@@deriving sexp, equal]

  module As_named (A : Equal.S) :
    Travesty.Traversable_types.S0
      with type t = A.t t
       and type Elt.t = A.t M.t = Travesty.Traversable.Make0 (struct
    type nonrec t = A.t t

    module Elt = struct
      type t = A.t M.t [@@deriving equal]
    end

    module On (M : Applicative.S) = struct
      module L = Travesty_base_exts.List.On (M)

      let map_m (x : t) ~(f : Elt.t -> Elt.t M.t) : t M.t =
        L.map_m x ~f:(fun (name, value) ->
            M.map ~f:(fun m' -> (m'.name, m'.value)) (f (make ~name value)))
    end
  end)
end
