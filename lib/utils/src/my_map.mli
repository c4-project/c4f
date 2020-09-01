(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Utilities for working with Base maps. *)

open Base

val map_with_keys :
     ('bk, 'bw) Map.comparator
  -> ('ak, 'av, 'aw) Map.t
  -> f:(key:'ak -> data:'av -> 'bk * 'bv)
  -> ('bk, 'bv, 'bw) Map.t Or_error.t
(** [map_with_keys cmp map ~f] maps [f] across the keys and values of [f],
    then tries to reconstitute a map from the result. *)

val find_or_error :
     ?sexp_of_key:('k -> Sexp.t)
  -> ?map_name:string
  -> ('k, 'v, 'w) Map.t
  -> 'k
  -> 'v Or_error.t
(** [find_or_error ?sexp_of_key ?map_name map key] behaves like
    [find map key], but, instead of returning [None] on failure, returns an
    [Or_error] error. By passing [sexp_of_key], one can add the key to the
    returned error; by passing [map_name], one can customise the name given
    to the map in the error. *)

val merge_with_overlap :
     ('k, 'v, 'w) Map.t list
  -> compare:('v -> 'v -> int)
  -> ('k, 'v, 'w) Map.t Or_error.t
(** [merge_with_overlap maps ~compare] merges [maps] into a single map,
    permitting overlap between two maps' entries where [compare] returns
    equality for the entries' values. It fails if there is not at least one
    map in [maps], or if two maps map the same key to different values. *)
