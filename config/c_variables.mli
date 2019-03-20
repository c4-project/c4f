(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

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

(** Interface for passing information about C variables to, and
   around, act.

    Several of act's components need auxiliary information about the
   variables of a C program or litmus test.  Most of this information
   corresponds to facts about a litmus test that get erased when it is
   converted to a compilable C file.

    For example, we may need to know which variables in a compilable C
   file correspond to shared variables in its litmus test, or the
   values the litmus test used to initialise those variables.

    This module contains various sub-modules for handling this
   auxiliary information.  *)

open Utils

(** Flag used to mark C variables with information about their scope. *)
module Scope : sig
  type t =
    | Unknown
    | Local
    | Global
  [@@deriving sexp, compare, equal]

  (** [is_global scope] is [true] if [scope] is (definitely) global. *)
  val is_global : t -> bool

  (** [is_local scope] is [true] if [scope] is (definitely) local. *)
  val is_local : t -> bool
end

(** Information about the initial value of C variables. *)
module Initial_value : sig
  type t = int option [@@deriving sexp, compare, equal]
end

(** A record containing all known information about a C variable.

    Records contain:
    - The intended scope of the variable (global or local);
    - The initial value of the variable, if known;
    - The thread ID to which the variable is known to be attached, if any.
*)
module Record : sig
  type t [@@deriving sexp, compare, equal]

  (** [tid record] gets [record]'s thread ID, if any. *)
  val tid : t -> int option

  (** [scope record] gets [record]'s scope. *)
  val scope : t -> Scope.t

  (** [initial_value record] gets [record]'s initial value, if any. *)
  val initial_value : t -> Initial_value.t

  (** [is_global record] is [true] if [record]'s scope is (definitely) global. *)
  val is_global : t -> bool

  (** [is_local record] is [true] if [record]'s scope is (definitely) local. *)
  val is_local : t -> bool
end

(** A map from C variable identifiers to their records. *)
module Map : sig
  type t = Record.t C_identifier.Map.t [@@deriving sexp, equal]

  (** {2 Constructors} *)

  (** [of_single_scope_map ?tid ?scope vars] lifts [vars] to a variable map,
      applying [scope] and [tid] to each variable. *)
  val of_single_scope_map
    : ?tid:int -> ?scope:Scope.t -> Initial_value.t C_identifier.Map.t -> t

  (** [of_single_scope_set ?tid ?scope vars] lifts [vars] to a variable map,
      applying [scope] and [tid] to each variable and assigning [None] as the
      initial value. *)
  val of_single_scope_set : ?tid:int -> ?scope:Scope.t -> C_identifier.Set.t -> t

  (** [merge_list maps] makes a variable-to-record map by merging
     [maps].  Merging happens in an arbitrary order, but respects the
     ordering on scopes. *)
  val merge_list : t list -> t

  (** [of_value_maps ~locals ~globals] makes a
       variable-to-record map by merging the locals map
       [locals] and globals map [globals]. *)
  val of_value_maps
    :  locals:Initial_value.t C_identifier.Map.t
    -> globals:Initial_value.t C_identifier.Map.t
    -> t

  (** [of_value_maps_opt ?locals ?globals ()] tries to make a
       variable-to-record map by merging the optional locals map
       [locals] and optional globals map [globals]. *)
  val of_value_maps_opt
    :  ?locals:Initial_value.t C_identifier.Map.t
    -> ?globals:Initial_value.t C_identifier.Map.t
    -> unit
    -> t option

  (** {3 Filtering variables} *)

  (** [vars_satisfying map ~f] extracts a set of all identifiers whose records in [map] satisfy [f]. *)
  val vars_satisfying : t -> f:(Record.t -> bool) -> C_identifier.Set.t

  (** [globals map] is [vars_satisfying map ~f:Record.is_global]. *)
  val globals : t -> C_identifier.Set.t

  (** [locals map] is [vars_satisfying map ~f:Record.is_local]. *)
  val locals : t -> C_identifier.Set.t
end
