(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Records of auxiliary Litmus information.

    This module describes a container for the parts of a Litmus test that
    aren't the program itself: the locations stanza, initial value map, and
    postcondition. Since de-litmusifying C tests removes this information *)

open Base

type 'const t [@@deriving equal, sexp]
(** Opaque type of auxiliary Litmus records. *)

(** {2 Constructors} *)

val make :
     ?locations:Act_common.C_id.t list
  -> ?init:(Act_common.C_id.t, 'const) List.Assoc.t
  -> ?postcondition:'const Postcondition.t
  -> unit
  -> 'const t
(** [make ?locations ?init ?postcondition ()] makes an auxiliary record with
    the given fields. *)

val empty : 'const t
(** [empty] is the empty auxiliary record. *)

(** {2 Accessors} *)

val locations : _ t -> Act_common.C_id.t list option
(** [locations aux] gets the computed location stanza of [aux], if any. *)

val init : 'const t -> (Act_common.C_id.t, 'const) List.Assoc.t
(** [init aux] gets the computed init block of [aux]. *)

val postcondition : 'const t -> 'const Postcondition.t option
(** [postcondition aux] gets the postcondition given in [aux], if any. *)

(** {2 Modifying an auxiliary record} *)

val add_global :
     'const t
  -> name:Act_common.C_id.t
  -> initial_value:'const
  -> 'const t Or_error.t
(** [add_global aux ~name ~initial_value] adds a global variable with name
    [name] and initial value [~initial_value] to the auxiliary record [aux]. *)

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
