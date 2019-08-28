(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Argument-parsing boilerplate for assembly commands. *)

open Core

(** Variant of {{!Standard_with_files} Standard_with_files} including the
    standard [act asm] arguments. *)
module Standard_asm : sig
  type t
  (** Opaque type of processed argument records. *)

  val rest : t -> Toplevel.Args.Standard.t Toplevel.Args.With_files.t
  (** [rest args] retrieves the argument record wrapped by [args]. *)

  val get : t Command.Param.t
  (** [get] is a [Command.Param.t] that describes how to get the standard
      assembly arguments at the command line. *)

  val aux_file : t -> string option
  (** [aux_file args] gets the path of a litmus aux file, if user supplied
      one. *)

  val target : t -> Toplevel.Asm_target.t
  (** [target args] gets either a defined assembly architecture, or a
      compiler ID. *)

  val sanitiser_passes :
    t -> Act_sanitiser.Pass_group.Selector.t Blang.t option
  (** [sanitiser_passes args] gets the Blang predicate, if any, supplied to
      filter the sanitiser pass selection. *)
end
