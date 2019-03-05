(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation
   files (the "Software"), to deal in the Software without
   restriction, including without limitation the rights to use, copy,
   modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

(** Configuration for the main compiler tester.

    This module exposes types used to configure the {{!Tester}Tester},
   `act`'s main multi-file compiler testing routine.

    This configuration, generally, isn't used in `act`'s other
    tools.  For act-wide configuration, see {{!Config}Config}. *)

open Base

(** Enumeration of different ways in which the tester can extract a C
    file and corresponding litmus test from its input witness. *)
module C_litmus_mode : sig
  type t =
    | Memalloy
      (** Assume that there are C/litmus files in the 'Litmus'
          subdirectory, and C files in the 'C' subdirectory. *)
    | Delitmusify
    (** Assume that there are C/litmus files in the input
        root, and that we must generate delitmusified versions ourselves. *)
end

type t
(** Opaque type of tester configuration.

    The items inside the configuration type used here are those that
   could change between different runs of the tester on the same set
   of machine configurations.  (At time of writing, this distinction
   is purely academic, but later versions of act might actually expose
   a way to run multiple tester jobs. *)

(** {2 Constructors} *)

val make
  :  fnames:string list
  -> in_root:Fpath.t
  -> out_root:Fpath.t
  -> compilers:Id.Set.t
  -> ?c_litmus_mode:C_litmus_mode.t
  -> unit
  -> t Or_error.t
(** [make ~fnames ~in_root ~out_root ~compilers
   ?c_litmus_mode ?timing_mode ()] constructs a set of tester
   configuration with the given parameters.  It fails if any of the
   parameters are invalid.

    The optional parameters have the following defaults:
    - [c_litmus_mode]: {{!C_litmus_mode.Memalloy}Memalloy}. *)

(** {2 Accessors} *)

val fnames : t -> string list
(** [fnames cfg] gets the filename list for [cfg]. *)

val in_root : t -> Fpath.t
(** [in_root cfg] gets the input root directory for [cfg]. *)

val out_root : t -> Fpath.t
(** [out_root cfg] gets the output root directory for [cfg]. *)

val compilers : t -> Id.Set.t
(** [compilers cfg] gets the compiler identifier set for [cfg]. *)

val c_litmus_mode : t -> C_litmus_mode.t
(** [c_litmus_mode cfg] gets the C litmus mode for [cfg]. *)
