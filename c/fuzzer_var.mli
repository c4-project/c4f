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

(** Fuzzer: variable records and maps

    This module defines the types that the fuzzer uses to store
    information about variables. *)

open Utils

(** Variable known-values *)
module Value : sig
  type t =
    | Known_int of int
end

(** Variable records *)
module Record : sig
  type t

  (** {3 Constructors} *)

  val make_existing_global : Mini.Type.t -> t
  (** [make_existing_global ty] makes a variable record for a global
      variable of type [ty] that already existed in the program before
      mutation. *)

  val make_existing_local : C_identifier.t -> t
  (** [make_existing_local name] makes a variable record for a local
     variable with name [name] that already existed in the program
     before mutation. *)

  val make_generated_global : ?initial_value:Value.t -> Mini.Type.t -> t
  (** [make_generated_global ?initial_value ty] makes a variable record for
      a fuzzer-generated global variable of type [ty] and with initial
      value [value]. *)

  (** {3 Predicates} *)

  val is_global : t -> bool
  (** [is_global vr] returns whether [vr] is a global variable. *)

  val is_atomic : t -> bool
  (** [is_atomic vr] returns whether [vr] is an atomic variable. *)

  val was_generated : t -> bool
  (** [was_generated vr] returns whether [vr] was generated by the fuzzer. *)

  (** {3 Properties} *)

  val ty : t -> Mini.Type.t option
  (** Gets the type of the variable, if known. *)

  (** {3 Actions} *)

  val erase_value : t -> t
  (** [erase_value record] erases the known-value field of [record].

      This should be done after involving [record] in any atomic
      actions that modify it. *)
end

(** Variable maps *)
module Map : sig
  type t = Record.t C_identifier.Map.t
  (** Variable maps associate C identifiers with records. *)

  (** {3 Constructors} *)

  val make_existing_var_map
    :  Mini.Type.t C_identifier.Map.t
    -> C_identifier.Set.t
    -> t
  (** [make_existing_var_map globals locals] expands a set of
      known-existing C variable names to a var-record map where each
      name is registered as an existing variable.

      Global registrations override local ones, in the case of
      shadowing. *)

  (** {3 Actions} *)

  val register_global
    :  ?initial_value:Value.t
    -> t
    -> C_identifier.t
    -> Mini.Type.t
    -> t
  (** [register_global ?initial_value map var ty] registers a
     generated global variable with name [var], type [ty], and
     optional known initial value [initial_value] in [map], returning
     the resulting new map. *)

  val erase_value : t -> var:C_identifier.t -> t
  (** [erase_value map ~var] erases the known-value field for any
     mapping for [var] in [map], returning the resulting new map.

      This should be done after involving [var] in any atomic actions
     that modify it. *)
end
