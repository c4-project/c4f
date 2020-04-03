(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Type of output into a filter or similar construct.

    This module contains a type that describes how a filter should dispose of
    its output---for example, into a local file, or on standard output. *)

open Base
open Stdio

(** Opaque type of outputs. *)
type t

(** @inline *)
include Io_types.Common with type t := t

(** {1 Constructing an output} *)

val file : Fpath.t -> t
(** [file fname] creates an output sink for a given filename. *)

val stdout : t
(** [stdout] is the standard output sink. *)

val temp_file : prefix:string -> ext:string -> Fpath.t
(** [temp_file ~prefix ~ext] creates a path for a temporary file with the
    given prefix and extension. *)

val temp : prefix:string -> ext:string -> t
(** [temp ~prefix ~ext] creates an output sink for a temporary file with the
    given prefix and extension. *)

(** {1 Consuming an output} *)

val as_input : t -> Input.t Or_error.t
(** [as_input o] tries to get an {{!Input.t} input} pointing to the same data
    as [o]. *)

val with_output : t -> f:(Out_channel.t -> 'a Or_error.t) -> 'a Or_error.t
(** [with_output o ~f] runs [f] connected to the output channel pointed to by
    [o]. *)

val with_output_opt :
  t option -> f:(Out_channel.t -> unit Or_error.t) -> unit Or_error.t
(** [with_output_opt o ~f] does nothing if [o] is None, and behaves as
    [with_output o'] if [o] is [Some o']. *)
