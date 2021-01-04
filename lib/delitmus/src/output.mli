(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** The output from a de-litmusification round. *)

open Import

(** Opaque type of de-litmusification output. *)
type t

(** {2 Constructors} *)

val make : program:unit Fir.Program.t -> aux:Aux.t -> t

(** {2 Accessors} *)

val program : t -> unit Fir.Program.t
(** [program output] gets the de-litmusified program. *)

val aux : t -> Aux.t
(** [aux output] gets the extracted (and potentially re-arranged) auxiliary
    Litmus information stored in [output]. *)

(* val c_variables : t -> C4f_common.C_variables.Map.t (** [c_variables
   output] gets a map containing information about each global and local
   variable contained in the de-litmusified program. *) *)
