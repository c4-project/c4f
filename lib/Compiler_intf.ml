(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

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

open Base

(** [Basic_spec] is the signature common to any sort of compiler
    specification, including [With_id] pairs.

    In practice, modules implementing this will either be [S_spec]
    or [S_spec.With_id]. *)
module type Basic_spec = sig
  (** [Mach] is some module for resolving references to the machine on
      which a compiler is located.  This can either be an inline
      machine specification module, or a more indirect form of
      reference. *)
  module Mach : Machine.Reference

  (** [t] is the opaque type of compiler specifications.
      To construct a [t], read one in as an S-expression;
      a proper constructor may appear in later revisions. *)
  type t

  (** [style c] gets the invocation style of [c]. *)
  val style : t -> string

  (** [emits c] gets the architecture emitted by [c]. *)
  val emits : t -> Id.t

  (** [cmd] gets the command used to invoke [c]. *)
  val cmd : t -> string

  (** [argv] gets any extra arguments to supply to [c]. *)
  val argv : t -> string list

  (** [herd c] gets whether Herd auto-running is enabled for [c]. *)
  val herd : t -> bool

  (** [machine] gets the machine reference for [c]. *)
  val machine : t -> Mach.t
end

(** [S_spec] is the interface of modules defining compiler
    specification types.

    In most cases, the 'correct' spec module to use is [Spec]. *)
module type S_spec = sig
  include Basic_spec

  (** [create ~enabled ~style ~emits ~cmd ~argv ~herd ~machine]
      creates a compiler spec with the given fields.

      These fields are subject to change, and as such [create] is an
      unstable API. *)
  val create
    :  enabled : bool
    -> style   : string
    -> emits   : Id.t
    -> cmd     : string
    -> argv    : string list
    -> herd    : bool
    -> machine : Mach.t
    -> t
  ;;

  (** We extend [With_id] to include all of the accessors from
     [Basic_spec]. *)
  module With_id : sig
    include Spec.S_with_id with type elt := t
    include Basic_spec with type t := t and module Mach := Mach
  end

  include Spec.S with type t := t and module With_id := With_id
end

(** [Basic] is the basic interface compilers must implement. *)
module type Basic = sig
  (** [test_args] is the set of arguments sent to the compiler to test
      that it works.  Usually, this will be some form of version
      command. *)
  val test_args : string list

  (** [compile_args ~args ~emits ~infile ~outfile] takes the set of
      arguments [args] the user supplied, the name of the architecture
      [emits] the compiler is going to emit, the input file
      [infile], and the output file [outfile], and produces a final
      argument vector to send to the compiler. *)
  val compile_args
    :  args    : string list
    -> emits   : Id.t
    -> infile  : string
    -> outfile : string
    -> string list
end

(** [S] is the outward-facing interface of compiler modules. *)
module type S = sig
  (** [test ()] tests that the compiler is working. *)
  val test : unit -> unit Or_error.t

  (** [compile ~infile ~outfile] runs the compiler on [infile],
      emitting assembly to [outfile] and returning any errors that arise. *)
  val compile : infile:Fpath.t -> outfile:Fpath.t -> unit Or_error.t
end
