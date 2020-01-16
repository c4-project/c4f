(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Type of low-level output destinations for the standard output of a
    program inside a {{!Runner_types.S} runner}. *)

open Base
open Stdio

(** Type of runner outputs. *)
type t =
  | Nowhere  (** Ignore standard output. *)
  | To_out_channel of Out_channel.t
      (** Copy standard output to an output channel. *)
  | To_buffer of Buffer.t  (** Copy standard output to a buffer. *)

(** {1 Producing runner outputs} *)

val stdout : t
(** [stdout] is [To_out_channel (Stdio.stdout)]. *)

(** {1 Sending data to runner outputs} *)

val to_stdoutf : t -> (Bytes.t -> int -> unit) Staged.t
(** [to_stdoutf out] converts [out] to a function that can serve as the
    [stdoutf] in a 'Low_level_process' call. *)

val output_lines : t -> string list -> unit
(** [output_lines out] outputs a list of lines to [out]. *)
