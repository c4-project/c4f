(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Storable module types *)

open Base
open Stdio

(** [Basic] is an interface to be implemented by anything using [Make]. *)
module type Basic = sig
  (** The type to load. *)
  type t

  val store_to_oc :
    ?path:string -> t -> dest:Out_channel.t -> unit Or_error.t
  (** [store_to_oc ?path oc] stores a [t] to an input channel [ic]. If [oc]
      comes from a file with a given path, [path] should be set to [Some x]
      where [x] is that path. *)
end

(** [S] is an interface for modules whose main type can be stored to a file. *)
module type S = sig
  include Basic

  val store : t -> dest:Output.t -> unit Or_error.t
  (** [store t ~dest] stores [t] to the output specified by [dest]. *)
end
