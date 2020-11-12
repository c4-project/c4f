(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Type of, and functions manipulating, the global ACT config set.

    This is the processed form of the configuration that ACT loads from
    act.conf (or other such files). *)

(** TODO(\@MattWindsor91): consider replacing this directly with
    [Act_fuzz_run.Config.t]. *)

(** Opaque type of global configuration records. *)
type t

(** {2 Construction and loading} *)

val make : ?fuzz:Act_fuzz_run.Config.t -> unit -> t
(** [make ?fuzz ()] constructs a global configuration record from the given
    pieces of configuration. At time of writing, there is only fuzzer
    configuration. *)

(** Module for loading the global configuration from an act.conf file. *)
module Load : Plumbing.Loadable_types.S with type t = t

(** {2 Projections} *)

val fuzz : t -> Act_fuzz_run.Config.t
(** [fuzz c] gets the fuzzer config, if any, to use for configuration [c]. *)
