(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Delitmus auxiliary record makers. *)

open Base
open Import

(** Given the basic shape of a delitmus runner, this functor constructs a
    one-function module that produces auxiliary records. *)
module Make (B : Runner_types.Basic) : sig
  val make_aux : Fir.Litmus.Test.t -> Aux.t Or_error.t
  (** [make_aux test] tries to extract an auxiliary record for [test]. This
      record details the various function and variable mappings that should
      be performed by the delitmusifier, and can be used both in the delitmus
      process to guide said mappings and afterwards to match the original
      test input to the delitmus output. *)
end
