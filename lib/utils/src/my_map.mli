(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

val map_with_keys :
     ('bk, 'bw) Map.comparator
  -> ('ak, 'av, 'aw) Map.t
  -> f:(key:'ak -> data:'av -> 'bk * 'bv)
  -> ('bk, 'bv, 'bw) Map.t Or_error.t
(** [map_with_keys cmp map ~f] maps [f] across the keys and values of [f],
    then tries to reconstitute a map from the result. *)
