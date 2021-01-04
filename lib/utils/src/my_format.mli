(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Miscellaneous formatting utilities. *)

open Base

val null_formatter : unit -> Formatter.t
(** [null_formatter ()] is a formatter that silently discards anything
    printed to it. *)

val pp_c_braces : 'v Fmt.t -> 'v Fmt.t
(** [pp_c_braces pi f i] wraps a vertical pretty-printer [pi] inside a
    C-style brace pair. *)

val pp_set : 'elem Fmt.t -> ('elem, 'cmp) Set.t Fmt.t
(** [pp_set pp_elem] pretty-prints prints a set, using {!pp_elem} as the
    element printer. *)

val pp_if : unit Fmt.t -> unit Fmt.t -> bool Fmt.t
(** [pp_if t f] formats a Boolean with [t] if true, or [f] if false. *)

val pp_or_error : 'elem Fmt.t -> 'elem Or_error.t Fmt.t
(** [pp_or_error f] lifts the formatter [f] over [Or_error]. *)

val poc : Stdio.Out_channel.t -> ('a, Formatter.t, unit) format -> 'a
(** [poc oc] is like [pr], but outputting via [oc]. *)

val fdump : Stdio.Out_channel.t -> 'a Fmt.t -> 'a -> unit
(** [fdump oc ppx x] dumps [x] to [oc] with a newline, using [ppx] to print
    it. *)

val odump : Plumbing.Output.t -> 'a Fmt.t -> 'a -> unit Or_error.t
(** [odump output ppx x] tries to dump [x] to [output] with a newline, using
    [ppx] to print it. *)
