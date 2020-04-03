(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Loadable module types *)

open Base
open Stdio

(** [Basic] is an interface to be implemented by anything using [Make]. *)
module type Basic = sig
  (** The type to load. *)
  type t

  val load_from_string : string -> t Or_error.t
  (** [load_from_string s] loads a [t] directly from a string [s]. *)

  val load_from_ic : ?path:string -> In_channel.t -> t Or_error.t
  (** [load_from_ic ?path ic] loads a [t] from an input channel [ic]. If [ic]
      comes from a file with a given path, [path] should be set to [Some x]
      where [x] is that path. *)
end

(** [S] is an interface for modules whose main type can be loaded from a
    file. *)
module type S = sig
  include Basic

  val load : Input.t -> t Or_error.t
  (** [load input] loads a [t] from the input source specified by [input]. *)
end
