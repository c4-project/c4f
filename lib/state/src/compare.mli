(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Comparing two simulation outputs.

    This module contains a function, {{!run} run}, for running
    'diff'-comparisons between two simulation outputs: an 'oracle' (generally
    from a C litmus test) and a 'subject' (generally from its compiled
    assembly).

    Each run takes, as additional parameters, partial mappings between the
    (litmus-style) state variable identifiers, and (uninterpreted string)
    values, mapping from the subject back to the oracle. *)

open Base

(** {1 The comparison result structure} *)

module Result : sig
  type t [@@deriving yojson_of]

  val domain : t -> Set.M(Act_common.Litmus_id).t

  val diff : t -> Diff.t

  val is_oracle_undefined : t -> bool

  val is_subject_undefined : t -> bool

  (** {2 Outputting results} *)

  val to_string : t -> string
  (** [to_string result] returns a human-readable string representing
      [result]. *)

  include Pretty_printer.S with type t := t
  (** We can also pretty-print diff results, with similar results to running
      [to_string]. *)

  val pp_json : t Fmt.t
  (** [dump_as_json f r] is a convenience function that outputs [r] on
      formatter [f] as (semi-human-readable) JSON. *)
end

(** {1 Running comparisons} *)

val run :
  oracle:Observation.t -> subject:Observation.t -> Result.t Or_error.t
(** [run ~oracle ~subject] runs the state differ on oracle [oracle] and
    subject [subject]. *)
