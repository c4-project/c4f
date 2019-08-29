(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Checking that an observation satisfies a Litmus postcondition. *)

open Base

(** {1 Result structures} *)
module Result : sig
  type t [@@deriving yojson_of]
end

(** {1 Running satisfiability queries} *)

val run :
  Observation.t -> post:string Act_litmus.Postcondition.t -> Result.t
(** [run obs ~post] checks to see if [post] is satisfiable by [obs]. [post]
    is a Litmus postcondition where each constant is a string (this being
    because the ACT state representation currently uses string values). *)
