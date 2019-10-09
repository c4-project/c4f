(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Abstract type of context passed into filter stages. *)

type 'aux t

(** {2 Constructors} *)

val make : aux:'aux -> input:Input.t -> output:Output.t -> 'aux t
(** [make ~aux ~input ~output] makes a filter context. *)

(** {2 Accessors} *)

val aux : 'aux t -> 'aux
(** [aux ctx] gets the auxiliary input information passed through [ctx]. *)

val input : _ t -> Input.t
(** [input ctx] gets the input descriptor passed through [ctx]. *)

val input_path_string : _ t -> string
(** [input_path_string ctx] gets the string description of the path, if any,
    associated with the input descriptor passed through [ctx]. *)

val output : _ t -> Output.t
(** [output ctx] gets the output descriptor passed through [ctx]. *)

(** {2 Transforming the auxiliary input} *)

module On_aux : Travesty.Traversable_types.S1 with type 'a t := 'a t
