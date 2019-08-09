(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

module type Basic = Plumbing.Loadable_types.S with type t = Output.t
(** Shorthand for the specific type of loadable module a simulator runner
    expects. *)

(** An extended form of {!Basic}. *)
module type S = sig
  include Basic

  val read_output_from_string : string -> Output.t
  (** [read_output_from_string s] runs the simulator's reader on [s],
      returning the output.

      We mainly intend this function to be used for testing. *)
end
