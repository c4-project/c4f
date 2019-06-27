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
  type t
  (** The type to load. *)

  val load_from_string : string -> t Or_error.t
  (** [load_from_string s] loads a [t] directly from a string [s]. *)

  val load_from_ic : ?path:string -> In_channel.t -> t Or_error.t
  (** [load_from_ic ?path ic] loads a [t] from an input channel [ic]. If
      [ic] comes from a file with a given path, [path] should be set to
      [Some x] where [x] is that path. *)
end

(** [S] is an interface for modules whose main type can be loaded from a
    file. *)
module type S = sig
  include Basic

  val load_from_isrc : Input.t -> t Or_error.t
  (** [load_from_isrc is] loads a [t] from an input source [is]. *)

  val load : path:Fpath.t -> t Or_error.t
  (** [load ~path] loads a [t] from a file named [path]. *)
end

(** {2 Chaining} *)

(** Signature of modules that explain how to post-process the result of a
    loadable. *)
module type Basic_chain = sig
  type src
  (** Type of the original loadable. *)

  type dst
  (** Type of the new loadable. *)

  val f : src -> dst Or_error.t
  (** [f src] is a potentially-failing transformation from [src] to a member
      of [dst]. *)
end
