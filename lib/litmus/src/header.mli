(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Records of auxiliary Litmus information.

    This module describes a container for the parts of a Litmus test that
    aren't the program itself: the name, locations stanza, initial value map,
    and postcondition. *)

open Base

(** Opaque type of Litmus headers. *)
type 'const t [@@deriving equal, sexp]

(** {1 Constructors} *)

val make :
     ?locations:Act_common.C_id.t list
  -> ?init:(Act_common.C_id.t, 'const) List.Assoc.t
  -> ?postcondition:'const Postcondition.t
  -> name:string
  -> unit
  -> 'const t
(** [make ?locations ?init ?postcondition ~name ()] makes a header with the
    given fields. *)

val empty : 'const t
(** [empty] is the empty header, with the empty string as its name. *)

(** {1 Accessors} *)

val name : _ t -> string
(** [name header] gets the test name of [header], if any. *)

val locations : _ t -> Act_common.C_id.t list option
(** [locations header] gets the computed location stanza of [header], if any. *)

val init : 'const t -> (Act_common.C_id.t, 'const) List.Assoc.t
(** [init header] gets the computed init block of [header]. *)

val postcondition : 'const t -> 'const Postcondition.t option
(** [postcondition header] gets the postcondition given in [header], if any. *)

(** {1 Modifying a header} *)

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

(** {1 Traversals} *)

(** We permit monadic bi-traversal over the C identifiers and constants in a
    litmus auxiliary record, potentially changing the constant type. *)
include
  Travesty.Bi_traversable_types.S1_right
    with type 'const t := 'const t
     and type left = Act_common.C_id.t

(** {1 Serialisation} *)

module Json (Const : sig
  type t

  include Pretty_printer.S with type t := t

  include Plumbing.Jsonable_types.S with type t := t

  val parse_post_string : string -> t Postcondition.t Or_error.t
end) : Plumbing.Jsonable_types.S with type t = Const.t t

(** {1 Applying changes to headers}

    This sub-module describes change sets for Litmus headers. These support
    commands like `act-c modify-header` that apply a set of specific
    modifications to the header of a Litmus test. *)

module Change_set : sig
  (** Do not use; should be [:=], but at time of writing PPXes refuse to let
      us use 4.08 syntax. *)
  type 'const hdr = 'const t

  (** Opaque type of individual changes. *)
  type 'const t

  val make :
       ?name:[`Keep | `Replace_with of string]
    -> ?postcondition:
         [`Keep | `Clear | `Replace_with of 'const Postcondition.t]
    -> unit
    -> 'const t
  (** [make ?name ?postcondition ()] makes a change set from the given [name]
      and [postcondition] directives (which default to [`Keep].) *)

  val apply : 'const t -> header:'const hdr -> 'const hdr
  (** [apply change ~header] applies [change] to [header]. *)
end
