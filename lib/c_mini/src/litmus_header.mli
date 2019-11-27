(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

(** A specialisation of {!Act_litmus.Header} that includes JSON serialisation
    and deserialisation. *)

type t = Constant.t Act_litmus.Header.t [@@deriving equal, yojson]

(** {1 JSON} *)

include Plumbing.Loadable_types.S with type t := t
(** Loading Litmus headers from JSON. *)

include Plumbing.Storable_types.S with type t := t
(** Storing Litmus headers to JSON. *)

(** {1 Change sets}

    This module contains helpers for making change sets over mini-C litmus
    headers. *)
module Change_set : sig
  type t = Constant.t Act_litmus.Header.Change_set.t

  val of_args :
       ?name:[`Keep | `Replace_with of string]
    -> ?postcondition:[`Keep | `Clear | `Replace_with of string]
    -> unit
    -> t Or_error.t
  (** [of_args ?set_name ?set_postcondition ()] tries to turn the given raw
      change arguments into a change set. *)
end

(** {1 Filters for manipulating test headers}

    These filters make use of the JSON form of {!t}. *)

module Filters : sig
  (** A filter for extracting the header of a C-mini Litmus test. *)
  module Dump :
    Plumbing.Filter_types.S with type aux_i = unit and type aux_o = unit

  (** A filter for modifying parts of the header of a C-mini Litmus test. *)
  module Modify :
    Plumbing.Filter_types.S
      with type aux_i = Change_set.t
       and type aux_o = unit

  (** A filter for replacing the header of a C-mini Litmus test. *)
  module Replace :
    Plumbing.Filter_types.S with type aux_i = t and type aux_o = unit
end
