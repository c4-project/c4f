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

(** Opaque type of global configuration records. *)
type t

(** {2 Construction and loading} *)

val make :
     ?defaults:Default.t
  -> ?fuzz:Act_fuzz_run.Config.t
  -> machines:Act_machine.Spec.Set.t
  -> unit
  -> t
(** [make ?defaults ?fuzz ~machines ()] constructs a global configuration
    record from the given pieces of configuration. *)

(** Module for loading the global configuration from an act.conf file. *)
module Load : Plumbing.Loadable_types.S with type t = t

(** {2 Projections} *)

val defaults : t -> Default.t
(** [defaults c] gets the defaults set specified by configuration [c]. *)

val fuzz : t -> Act_fuzz_run.Config.t
(** [fuzz c] gets the fuzzer config, if any, to use for configuration [c]. *)

val machines : t -> Act_machine.Spec.Set.t
(** [machines c] gets the set of all active machines in configuration [c]. *)
