(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

(** [Common] describes operations common to both input sources and output
    sinks. *)
module type Common = sig
  type t

  (** {2 Path conversion} *)

  (** {3 From paths} *)

  val of_fpath : Fpath.t -> t
  (** [of_fpath n] returns a source or sink over the file path [n]. *)

  val of_fpath_opt : Fpath.t option -> t
  (** [of_fpath_opt n] returns [of_fpath n'] if [n] is [Some n'], and the
      standard stream otherwise. *)

  val of_string_opt : string option -> t Or_error.t
  (** [of_string_opt n] tries to parse [n] as a file path if it is [Some n],
      and returns the standard stream otherwise. *)

  val of_string : string -> t Or_error.t
  (** [of_string n] behaves as {!of_string_opt}, but maps "-" to [None]
      (standard stream) and all other paths [p] to [Some p] (file). *)

  (** {3 To paths} *)

  val to_fpath_opt : t -> Fpath.t option
  (** [to_fpath t] returns [Some f] if [t] is a file with path [f], and
      [None] otherwise. *)

  val to_fpath_err : t -> Fpath.t Or_error.t
  (** [to_fpath_err t] behaves like {!to_file}, but raises a mildly
      descriptive error on failure. *)

  val to_string_opt : t -> string option
  (** [to_string_opt t] behaves like {!to_file}, but returns any result as a
      string rather than a [Fpath.t]. *)

  val to_string : t -> string
  (** [to_string t] returns a descriptive string representation of [t]. *)

  include Pretty_printer.S with type t := t
end
