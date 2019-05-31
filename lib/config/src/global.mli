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
     ?cpp:Cpp.t
  -> ?defaults:Ast.Default.t list
  -> ?fuzz:Fuzz.t
  -> machines:Act_compiler.Machine_spec.Set.t
  -> unit
  -> t
(** [make ?cpp ?defaults ?fuzz ~machines ()] constructs a global
    configuration record from the given pieces of configuration. *)

(** Module for loading the global configuration from an act.conf file. *)
module Load : Act_utils.Loadable_intf.S with type t = t

(** {2 Projections} *)

include Global_types.S with type t := t
