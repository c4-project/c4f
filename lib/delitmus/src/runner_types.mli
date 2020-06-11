(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Module types for {!Runner} and its related modules. *)

open Base

(** Input signature for delitmus runners. *)
module type Basic = sig
  val global_mapping : int -> Var_map.Mapping.t
  (** [global_mapping index] returns the intended mapping for a global
      variable that forms the [index]th variable declaration. *)

  val local_mapping : int -> Var_map.Mapping.t
  (** [local_mapping index] returns the intended mapping for a local variable
      that forms the [index]th variable declaration. *)

  val impl_suffix : string option
  (** [impl_suffix] is, if given, the suffix to append to the thread function
      names. This is intended to facilitate generating 'stub' litmus tests
      whose bodies bounce into delitmusified bodies. *)

  val qualify_locals : bool
  (** [qualify_locals] is [true] if local variable references should be
      qualified with their thread ID, and [false] otherwise. *)

  (** [Function] is a function rewriter. *)
  module Function : Function_rewriter.S
end

(** Main signature for delitmus runners. *)
module type S = sig
  val run : Act_fir.Litmus.Test.t -> Output.t Or_error.t
  (** [run test] runs a delitmus operation on [test]. The exact nature of the
      delitmusified output depends on the way in which the delitmusifier was
      configured. *)
end
