(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

module Parametric = struct
  type 'a t = {name: C_id.t; value: 'a}
  [@@deriving sexp, compare, equal, fields, quickcheck]
end

include Parametric

let make (value : 'a) ~(name : C_id.t) : 'a t = Fields.create ~value ~name

let seq_of_alist (xs : (C_id.t, 'a) List.Assoc.t) : 'a t Sequence.t =
  xs |> Sequence.of_list
  |> Sequence.map ~f:(fun (name, value) -> make value ~name)

let list_of_alist (xs : (C_id.t, 'a) List.Assoc.t) : 'a t list =
  xs |> seq_of_alist |> Sequence.to_list

let alist_of_seq (xs : 'a t Sequence.t) : (C_id.t, 'a) List.Assoc.t =
  xs |> Sequence.map ~f:(fun t -> (name t, value t)) |> Sequence.to_list

let alist_of_list (xs : 'a t list) : (C_id.t, 'a) List.Assoc.t =
  xs |> Sequence.of_list |> alist_of_seq

module BT :
  Travesty.Bi_traversable_types.S1_right
    with type 'r t := 'r t
     and type left = C_id.t = Travesty.Bi_traversable.Make1_right (struct
  type nonrec 'r t = 'r t

  type left = C_id.t

  module On_monad (M : Monad.S) = struct
    let bi_map_m (n : 'r1 t) ~(left : left -> left M.t)
        ~(right : 'r1 -> 'r2 M.t) : 'r2 t M.t =
      M.Let_syntax.(
        let%map name = left (name n) and value = right (value n) in
        make value ~name)
  end
end)

include BT

module Alist = struct
  type 'a t = ((C_id.t * 'a) list[@sexp.list]) [@@deriving sexp, equal]

  module As_named (A : Equal.S) :
    Travesty.Traversable_types.S0
      with type t = A.t t
       and type Elt.t = A.t Parametric.t = Travesty.Traversable.Make0 (struct
    type nonrec t = A.t t

    module Elt = struct
      type t = A.t Parametric.t [@@deriving equal]
    end

    module On_monad (M : Monad.S) = struct
      module L = Travesty_base_exts.List.On_monad (M)

      let map_m (x : t) ~(f : Elt.t -> Elt.t M.t) : t M.t =
        L.map_m x ~f:(fun (name, value) ->
            M.Let_syntax.(
              let m = make ~name value in
              let%map m' = f m in
              (m'.name, m'.value)) )
    end
  end)
end
