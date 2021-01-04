(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

(** A specialisation of {!Act_litmus.Header} that includes JSON serialisation
    and deserialisation. *)

type t = Act_fir.Constant.t Act_litmus.Header.t [@@deriving equal, yojson]

(** {1 JSON} *)

(** Loading Litmus headers from JSON. *)
include Plumbing.Loadable_types.S with type t := t

(** Storing Litmus headers to JSON. *)
include Plumbing.Storable_types.S with type t := t

(** {1 Change sets}

    This module contains helpers for making change sets over FIR litmus
    headers. *)
module Change_set : sig
  type t = Act_fir.Constant.t Act_litmus.Header.Change_set.t

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
  val run_dump : Plumbing.Input.t -> Plumbing.Output.t -> unit Or_error.t
  (** [run_dump input output] reads a Litmus/C test in through [input], and
      dumps the header in JSON form on [output]. *)

  val run_modify :
       Plumbing.Input.t
    -> Plumbing.Output.t
    -> changes:Change_set.t
    -> unit Or_error.t
  (** [run_modify input output ~change_set] reads a Litmus/C test in through
      [input], makes the changes mentioned in [change_set] to its header,
      then dumps the modified test on [output]. *)

  val run_replace :
    Plumbing.Input.t -> Plumbing.Output.t -> replacement:t -> unit Or_error.t
  (** [run_modify input output ~replacement] reads a Litmus/C test in through
      [input], replaces its header with [replacement], then dumps the
      modified test on [output]. *)
end
