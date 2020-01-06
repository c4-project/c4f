(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Top-level backend resolution etc. *)

open Base

(** {1 Filtered lookup of fully-qualified backend specs} *)

(** [Lookup] permits look-up of backends with, at the moment of writing, no
    presence testing. (This may change later.) *)
module Lookup : sig
  include Act_machine.Lookup_types.S_backend

  val lookup_in_cfg :
       Act_common.Id.t
    -> cfg:Act_config.Global.t
    -> Act_machine.Qualified.Backend.t Or_error.t
  (** [lookup_in_cfg fqid ~cfg] looks up the fully qualified backend ID
      [fqid] in the specs, and using the defaults, given by [cfg]. *)
end

(** {1 Resolution into runner modules} *)

val resolve :
     Act_machine.Qualified.Backend.t
  -> (module Act_backend.Instance_types.S) Or_error.t
(** [resolve spec] resolves [spec] using this module's built-in backend table
    to look up backends. *)

val lookup_and_resolve_in_cfg :
     Act_common.Id.t
  -> cfg:Act_config.Global.t
  -> (module Act_backend.Instance_types.S) Or_error.t
(** [lookup_and_resolve_in_cfg fqid ~cfg] composes {!Lookup.lookup_in_cfg}
    and {!resolve}. *)
