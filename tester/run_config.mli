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

(** Per-run configuration for the compiler tester.

    This module exposes types used to configure the {{!Tester}Tester},
   `act`'s main multi-file compiler testing routine.

    This configuration, generally, isn't used in `act`'s other
    tools.  For act-wide configuration, see {{!Config}Config}. *)

open Base

type t
(** Opaque type of tester configuration.

    The items inside the configuration type used here are those that
   could change between different runs of the tester on the same set
   of machine configurations.  (At time of writing, this distinction
   is purely academic, but later versions of act might actually expose
   a way to run multiple tester jobs. *)

(** {2 Constructors} *)

val make
  :  output_root:Fpath.t
  -> compilers:Config.Id.Set.t
  -> input_mode:Input_mode.t
  -> t Or_error.t
(** [make ~fnames ~output_root ~compilers ~input_mode ()] constructs a
   set of tester configuration with the given parameters.  It fails if
   any of the parameters are invalid. *)

(** {2 Accessors} *)

val output_root : t -> Fpath.t
(** [output_root cfg] gets the output root directory for [cfg]. *)

val compilers : t -> Config.Id.Set.t
(** [compilers cfg] gets the compiler identifier set for [cfg]. *)

val input_mode : t -> Input_mode.t
(** [input_mode cfg] gets the input mode for [cfg]. *)
