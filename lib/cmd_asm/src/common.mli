(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Common infrastructure for [act asm] commands.

    This module sits on top of {{!Common} Common}. *)

open Base

module Input : sig
  type t
  (** Opaque type of assembly command input. *)

  val act_config : t -> Act_config.Act.t

  val pb_input : t -> Plumbing.Input.t

  val pb_output : t -> Plumbing.Output.t

  val output : t -> Act_common.Output.t

  val sanitiser_passes : t -> Set.M(Act_sanitiser.Pass_group).t

  val target : t -> Act_machine.Target.t

  val c_litmus_aux : t -> Act_delitmus.Aux.t
  (** [c_litmus_aux in] gets the auxiliary information associated with any C
      litmus test that was previously processed into the input to this
      assembly command. *)

  val make_job_input :
    t -> (Act_delitmus.Aux.t -> 'cfg) -> 'cfg Act_asm.Job.t
end

val lift_command :
     Args.Standard_asm.t
  -> f:(Input.t -> unit Or_error.t)
  -> default_passes:Set.M(Act_sanitiser.Pass_group).t
  -> unit
