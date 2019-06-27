(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Type of, and functions manipulating, the global ACT config set.

    This is the processed form of the configuration that ACT loads from
    act.conf (or other such files). *)

type t
(** Opaque type of global configuration records. *)

(** {2 Construction and loading} *)

val make :
     ?defaults:Default.t
  -> ?fuzz:Fuzz.t
  -> machines:Act_machine.Spec.Set.t
  -> unit
  -> t
(** [make ?defaults ?fuzz ~machines ()] constructs a global configuration
    record from the given pieces of configuration. *)

module Load : Plumbing.Loadable_types.S with type t = t
(** Module for loading the global configuration from an act.conf file. *)

(** {2 Projections} *)

include Global_types.S with type t := t
