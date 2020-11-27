(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Helper functors for building {{!Jsonable_types} JSONable} modules. *)

open Base

(** {1 Helpers for sets} *)

module Set : sig
  val yojson_of_set : ('x -> Yojson.Safe.t) -> ('x, _) Set.t -> Yojson.Safe.t
  (** [yojson_of_set yojson_of_x xs] converts a set [xs] into a JSON list
      using [yojson_of_x] to convert values into JSON. *)

  val set_of_yojson :
       (module Comparator.S with type t = 'x and type comparator_witness = 'c)
    -> (Yojson.Safe.t -> 'x)
    -> Yojson.Safe.t
    -> ('x, 'c) Set.t
  (** [set_of_yojson x_of_yojson j] converts a JSON list [j] into a set using
      [x_of_yojson] to convert value JSON into values. It raises exceptions
      on failure. *)

  (** [Make] makes a standard JSON conversion module from a set, given fixed
      bi-directional value-to-JSON conversions. *)
  module Make (V : sig
    include Jsonable_types.S

    include Comparable.S with type t := t
  end) : Jsonable_types.S with type t = Set.M(V).t
end

(** {1 Helpers for associative lists} *)

module Alist : sig
  val yojson_of_alist :
       ('k -> string)
    -> ('v -> Yojson.Safe.t)
    -> ('k, 'v) List.Assoc.t
    -> Yojson.Safe.t
  (** [yojson_of_alist string_of_k yojson_of_v xs] converts an association
      list [xs] into a JSON object using [string_of_k] to convert keys into
      strings and [yojson_of_v] to convert values into JSON. *)

  val alist_of_yojson :
       (string -> 'k)
    -> (Yojson.Safe.t -> 'v)
    -> Yojson.Safe.t
    -> ('k, 'v) List.Assoc.t
  (** [alist_of_yojson k_of_string v_of_yojson j] converts a JSON object [j]
      into an association list using [k_of_string] to convert strings into
      keys and [v_of_yojson] to convert value JSON into values. It raises
      exceptions on failure. *)

  val alist_of_yojson' :
       (string -> 'k)
    -> (Yojson.Safe.t -> ('v, string) Result.t)
    -> Yojson.Safe.t
    -> (('k, 'v) List.Assoc.t, string) Result.t
  (** [alist_of_yojson' k_of_string v_of_yojson' j] behaves as
      {!alist_of_yojson}, but both [v_of_yojson'] and itself return
      string-error results. *)

  (** [Make] makes a standard JSON conversion module from an associative
      list, given fixed bi-directional key-to-string and value-to-JSON
      conversions. *)
  module Make (K : Stringable.S) (V : Jsonable_types.S) :
    Jsonable_types.S with type t = (K.t, V.t) List.Assoc.t
end

module Make_map (K : sig
  type t

  include Comparable.S with type t := t

  include Stringable.S with type t := t
end)
(V : Jsonable_types.S) : Jsonable_types.S with type t = V.t Map.M(K).t

(** Lifts stringable types to json-able types, where conversion is to/from
    strings. *)
module Of_stringable (E : Stringable.S) : Jsonable_types.S with type t = E.t

(** Strings packaged up as a json-able type. *)
module String : Jsonable_types.S with type t = string

(** Lifts a json-able type over options. *)
module Option (B : Jsonable_types.S) :
  Jsonable_types.S with type t = B.t option
