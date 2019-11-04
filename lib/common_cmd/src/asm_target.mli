(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** High-level form of `asm` command targets. *)

open Base

type t = Act_common.Id.t Act_machine.Target.t
(** Type of top-level command targets, as taken from the command line. *)

val resolve :
     t
  -> cfg:Act_config.Global.t
  -> Act_machine.Qualified.Compiler.t Act_machine.Target.t Or_error.t
(** [resolve target ~cfg] passes through [target] if it's a direct
    architecture reference; if it's a compiler ID, it tries to look up that
    ID in [cfg], resolving it to a compiler spec. *)
