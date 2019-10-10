(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Records of auxiliary Litmus information.

    This module describes a container for the parts of a Litmus test that
    aren't the program itself: the name, locations stanza, initial value map, and
    postcondition. *)

open Base

type 'const t [@@deriving equal, sexp]
(** Opaque type of Litmus headers. *)

(** {2 Constructors} *)

val make :
     ?locations:Act_common.C_id.t list
  -> ?init:(Act_common.C_id.t, 'const) List.Assoc.t
  -> ?postcondition:'const Postcondition.t
  -> name:string
  -> unit
  -> 'const t
(** [make ?locations ?init ?postcondition ~name ()] makes a header with
    the given fields. *)

val empty : 'const t
(** [empty] is the empty header, with the empty string as its name. *)

(** {2 Accessors} *)

val name : _ t -> string
(** [name header] gets the test name of [header], if any. *)

val locations : _ t -> Act_common.C_id.t list option
(** [locations header] gets the computed location stanza of [header], if any. *)

val init : 'const t -> (Act_common.C_id.t, 'const) List.Assoc.t
(** [init header] gets the computed init block of [header]. *)

val postcondition : 'const t -> 'const Postcondition.t option
(** [postcondition header] gets the postcondition given in [header], if any. *)

(** {2 Modifying a header} *)

val add_global :
     'const t
  -> name:Act_common.C_id.t
  -> initial_value:'const
  -> 'const t Or_error.t
(** [add_global aux ~name ~initial_value] adds a global variable with name
    [name] and initial value [~initial_value] to the auxiliary record [aux]. *)

val map_name : 'const t -> f:(string -> string) -> 'const t
(** [map_name header ~f] maps [f] over the name of [header]. *)

val map_tids : 'const t -> f:(int -> int) -> 'const t
(** [map_tids aux ~f] maps [f] over all of the thread IDs in [aux]
    (specifically, in [aux]'s postcondition). *)

(** {2 Traversals} *)

(** We permit monadic bi-traversal over the C identifiers and constants in a
    litmus auxiliary record, potentially changing the constant type. *)
include
  Travesty.Bi_traversable_types.S1_right
    with type 'const t := 'const t
     and type left = Act_common.C_id.t

(** {2 Serialisation} *)

module Json (Const : sig
  type t

  include Pretty_printer.S with type t := t

  include Plumbing.Jsonable_types.S with type t := t

  val parse_post_string : string -> t Postcondition.t Or_error.t
end) : Plumbing.Jsonable_types.S with type t = Const.t t
