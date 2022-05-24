(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

(** Litmus-style identifiers.

    These identifiers extend C-style identifiers with an optional thread
    identifier, and capture the type of identifier seen in Litmus
    postconditions. *)

(** Opaque type of litmus-style identifiers. *)
type t [@@deriving compare, sexp, quickcheck]

(** {1 Constructors} *)

val make : scope:Scope.t -> id:C_id.t -> t
(** [make ~scope ~id] constructs a Litmus-style identifier from the given
    [scope] and [id]. *)

val try_parse : string -> t Or_error.t
(** [try_parse str] tries to parse [str] as a Litmus identifier. For an
    exception-based alternative, use {!of_string}. *)

(** {3 Global identifiers} *)

val global : C_id.t -> t
(** [global id] creates a global identifier. *)

val global_of_string : string -> t Or_error.t
(** [global_of_string str] tries to create a global identifier from [str]. It
    fails if [str] isn't a valid C identifier. *)

val global_of_string_exn : string -> t
(** [global_of_string_exn str] is [global_of_string], but throws an exception
    on failure. *)

(** {3 Local identifiers} *)

val local : int -> C_id.t -> t
(** [local tid id] creates a local identifier with the given thread ID. *)

(** {2 Modifying an identifier} *)

val map_tid : t -> f:(int -> int) -> t
(** [map_tid id ~f] maps [f] over any thread identifier in [id]. *)

(** {2 Accessors} *)

val as_global : t -> C_id.t option
(** [as_global id] gets [Some cid] if [id] is the global identifier [cid], or
    [None] otherwise. *)

val as_local : t -> (int * C_id.t) option
(** [as_local id] gets [Some (tid, cid)] if [id] is the local identifier
    [cid] in thread [tid], or [None] otherwise. *)

val variable_name : t -> C_id.t
(** [variable_name id] gets the underlying variable name of [id]. *)

val tid : t -> int option
(** [tid id] gets [id]'s thread identifier, if it has one. *)

val scope : t -> Scope.t
(** [scope id] extracts the scope information from [id]. *)

val to_memalloy_id : t -> C_id.t
(** [to_memalloy_id id] converts [id] to the corresponding memalloy
    executable-C global variable name.

    This is [x] where [id = Global x], and ["tXY"] where [id = Local (X, Y)]. *)

(** {3 Predicates} *)

val is_global : t -> bool
(** [is_global id] is [true] if [id] is a global identifier, or [false]
    otherwise. *)

val is_local : t -> bool
(** [is_local id] is [true] if [id] is a local identifier, or [false]
    otherwise. *)

val is_in_local_scope : t -> from:int -> bool
(** [is_in_local_scope id ~from] asks whether [id] is in scope from the point
    of view of the thread with ID [from]. This is true if, and only if, [id]
    is global or is a local with ID [from]. *)

val is_in_scope : t -> scope:Scope.t -> bool
(** [is_in_scope id ~scope] asks whether [id] is in scope from the point of
    view of the scope [scope]. If [scope] is global, [id] must be global;
    else, [id] must either be global, or a local with the same ID as that of
    the scope. *)

(** {2 Interface implementations} *)

(** Litmus identifiers can be converted to and from strings. Note that
    conversion from strings can fail if the C identifier parts don't obey C
    identifier validation. *)
include Stringable.S with type t := t

(** Litmus-style identifiers are trivially serialisable to, and
    deserialisable from, JSON; we just encode them as strings. *)
include Plumbing.Jsonable_types.S with type t := t

(** Litmus identifiers can be pretty-printed. *)
include Pretty_printer.S with type t := t

(** Litmus identifiers suit various comparable scenarios, such as map keys. *)
include Comparable.S with type t := t

(** Monadic traversal over the C identifier part of a Litmus identifier. *)
module On_c_identifiers :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = C_id.t

(** Helpers for parsing, and handling, associative lists over litmus IDs. *)
module Assoc : sig
  type nonrec 'a t = (t, 'a) List.Assoc.t

  val try_parse :
       string list
    -> value_parser:(string option -> 'a Or_error.t)
    -> 'a t Or_error.t
  (** [try_parse strs ~value_parser] tries to parse the string list [strs],
      where each string is of the form [id=value], as an associative list
      with litmus ID keys and values interpreted by [value_parser]. *)
end
