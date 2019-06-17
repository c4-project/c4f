(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** The output from a de-litmusification round. *)

(** Opaque type of de-litmusification output. *)
type t

(** {2 Constructors} *)

val make : program:Act_c.Mini.Program.t -> aux:Aux.t -> t

(** {2 Accessors} *)

val program : t -> Act_c.Mini.Program.t
(** [program output] gets the de-litmusified program. *)

val aux : t -> Aux.t
(** [aux output] gets the extracted (and potentially re-arranged) auxiliary
    Litmus information stored in [output]. *)

(* val c_variables : t -> Act_common.C_variables.Map.t (** [c_variables
   output] gets a map containing information about each global and local
   variable contained in the de-litmusified program. *) *)
