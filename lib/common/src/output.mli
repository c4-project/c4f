(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Bundle of formatters used to output information in the middle of act
    execution. *)

open Base

(** Opaque type of output contexts. *)
type t

(** {1 Constructors} *)

val make : verbose:bool -> warnings:bool -> t
(** [make ~verbose ~warnings] makes a [t] from various reporting flags. *)

val silent : unit -> t
(** [silent ()] makes a [t] that ignores anything sent to it. *)

(** {1 Usage} *)

val is_verbose : t -> bool
(** [is_verbose o] gets whether [o] is in verbose mode; this can affect the
    choice of pretty-printers, for instance. *)

val pv : t -> ('a, Formatter.t, unit) format -> 'a
(** [pv output fmt] prints onto [output]'s 'verbose' output. *)

val pw : t -> ('a, Formatter.t, unit) format -> 'a
(** [pw output fmt] prints onto [output]'s 'warning' output. *)

val pe : t -> ('a, Formatter.t, unit) format -> 'a
(** [pe output fmt] prints onto [output]'s 'error' output (usually stderr). *)

(** {1 Common output forms} *)

val print_error : t -> unit Or_error.t -> unit
(** [print_error o u] prints any top-level errors represented by [u] to [o]'s
    error formatter. *)
