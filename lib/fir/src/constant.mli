(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** FIR constants.

    In ACT's FIR, constants are either integers or Booleans. The latter
    corresponds to the pseudo-constants 'true' and 'false' in C99. *)

open Base

(** Opaque type of constants. *)
type t [@@deriving compare, equal, sexp, quickcheck]

include Comparable.S with type t := t

include Plumbing.Jsonable_types.S with type t := t

include Pretty_printer.S with type t := t

(** {2 Constructors} *)

val bool : bool -> t
(** [bool b] lifts the Boolean value [b] to a constant. *)

val int : int -> t
(** [int b] lifts the integer value [i] to a constant. *)

val zero_of_type : Type.t -> t
(** [zero_of_type ty] gets the appropriate zero value for the type [ty]. *)

(** {3 Shorthand for specific values} *)

val truth : t
(** [truth] is the 'true' Boolean constant. *)

val falsehood : t
(** [falsehood] is the 'false' Boolean constant. *)

(** {2 Queries} *)

val is_bool : t -> bool
(** [is_bool k] is true if [k] is a Boolean constant. *)

val is_int : t -> bool
(** [is_int k] is true if [k] is an integer constant. *)

val reduce : t -> int:(int -> 'a) -> bool:(bool -> 'a) -> 'a
(** [reduce k] destructs [k] using [int] if [k] is an integer constant and
    [bool] if it is a Boolean constant. *)

(** {3 Extracting constants of specific types} *)

val as_bool : t -> bool Or_error.t
(** [as_bool k] returns [Some b] if [k] is a Boolean constant [b]; and an
    error otherwise. *)

val as_int : t -> int Or_error.t
(** [as_int k] returns [Some i] if [k] is an integer constant [i]; and an
    error otherwise. *)

(** {2 Type checking}

    Since constant checking never fails and doesn't require an environment,
    we {i don't} implement {{!Types.S_type_checkable} S_type_checkable}. *)

val type_of : t -> Type.t
(** [type_of k] gets the type of constant [k]. *)

(** {2 Specific Quickcheck generators} *)

val gen_int32 : t Base_quickcheck.Generator.t
(** [gen_int32] generates integer constants in the 32-bit range only. *)

val gen_bool : t Base_quickcheck.Generator.t
(** [gen_bool] generates Boolean constants. *)

val quickcheck_generator : t Base_quickcheck.Generator.t
(** [quickcheck_generator] generates Boolean and 32-bit integer constants. *)
