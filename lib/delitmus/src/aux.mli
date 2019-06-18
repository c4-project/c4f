(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Auxiliary information generated by a de-litmusification round.

    This combines both the auxiliary information from the litmus test itself,
    as well as information about the variables and thread IDs that were in
    use in the body of the Litmus test. *)

type t [@@deriving equal]

(** {2 Constructors} *)

val make :
  ?litmus_aux:Act_c.Mini.Constant.t Act_litmus.Aux.t
  -> var_map:Var_map.t
  -> unit
  -> t

val empty : t

(** {2 Accessors} *)

val litmus_aux : t -> Act_c.Mini.Constant.t Act_litmus.Aux.t

val var_map : t -> Var_map.t

val symbols : t -> string list

