(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Path flags.

    Path flags contain information about the context into which a path
    targets, for instance 'is in atomic block' or 'is in dead code'. These
    flags are produced as a path gets generated, and used to check the path
    against a {!Path_filter}. *)

open Base

(** Type of path flags. *)
type t =
  | In_loop  (** The path passes through a loop body. *)
  | In_dead_code  (** The path passes through a dead-code block. *)
  | In_atomic  (** The path passes through an atomic block. *)

include Act_utils.Enum_types.Extension_table with type t := t

(** {1 Acquiring path flags} *)

val flags_of_flow :
  Act_fir.Statement_class.Flow.t -> (t, comparator_witness) Set.t
(** [flags_of_flow c] gets the path flags that passing through a flow
    construct with class [c] will enable. *)

val flags_of_metadata : Metadata.t -> (t, comparator_witness) Set.t
(** [flags_of_metadata m] gets the path flags that passing through a
    construct with metadata [m] will enable. *)
