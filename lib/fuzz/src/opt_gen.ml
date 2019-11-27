(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

type 'a t = 'a Base_quickcheck.Generator.t option

include Applicative.Make_using_map2 (struct
  type nonrec 'a t = 'a t

  let return (type a) (x : a) : a t =
    Option.return (Base_quickcheck.Generator.return x)

  let map' (type a b) (x : a t) ~(f : a -> b) : b t =
    Option.map x ~f:(Base_quickcheck.Generator.map ~f)

  let map = `Custom map'

  let map2 (type a b c) (x : a t) (y : b t) ~(f : a -> b -> c) : c t =
    Option.map2 x y ~f:(Base_quickcheck.Generator.map2 ~f)
end)

let union (type a) (gens : a t list) : a t =
  Act_utils.My_list.guard_if_empty ~f:Base_quickcheck.Generator.union
    (List.filter_opt gens)
