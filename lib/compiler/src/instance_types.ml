(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to permit
   persons to whom the Software is furnished to do so, subject to the
   following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
   NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
   DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
   OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
   USE OR OTHER DEALINGS IN THE SOFTWARE. *)

open Base
open Act_common

(** [Basic_spec] is the signature common to any sort of compiler
    specification, including [With_id] pairs.

    In practice, modules implementing this will either be [S_spec] or
    [S_spec.With_id]. *)
module type Basic_spec = sig
  (** [Mach] is some module for resolving references to the machine on which
      a compiler is located. This can either be an inline machine
      specification module, or a more indirect form of reference. *)
  module Mach : Machine_types.Reference

  (** [t] is the opaque type of compiler specifications. To construct a [t],
      read one in as an S-expression; a proper constructor may appear in
      later revisions. *)
  type t

  val style : t -> string
  (** [style c] gets the invocation style of [c]. *)

  val emits : t -> Id.t
  (** [emits c] gets the architecture emitted by [c]. *)

  val cmd : t -> string
  (** [cmd] gets the command used to invoke [c]. *)

  val argv : t -> string list
  (** [argv] gets any extra arguments to supply to [c]. *)

  val herd : t -> bool
  (** [herd c] gets whether Herd auto-running is enabled for [c]. *)

  val machine : t -> Mach.t
  (** [machine] gets the machine reference for [c]. *)
end

(** [S_spec] is the interface of modules defining compiler specification
    types.

    In most cases, the 'correct' spec module to use is [Spec]. *)
module type S_spec = sig
  include Basic_spec

  val make :
       ?machine:Mach.t
    -> ?argv:string list
    -> enabled:bool
    -> style:string
    -> emits:Id.t
    -> cmd:string
    -> herd:bool
    -> unit
    -> t
  (** [make ?machine ?argv ~enabled ~style ~emits ~cmd ~herd ()] creates a
      compiler spec with the given fields.

      These fields are subject to change, and as such [make] is an unstable
      API. *)

  (** We extend [With_id] to include all of the accessors from [Basic_spec]. *)
  module With_id : sig
    include Spec.S_with_id with type elt := t

    include Basic_spec with type t := t and module Mach := Mach
  end

  include Spec.S with type t := t and module With_id := With_id
end

(** [Basic] is the basic interface compilers must implement. *)
module type Basic = sig
  val test_args : string list
  (** [test_args] is the set of arguments sent to the compiler to test that
      it works. Usually, this will be some form of version command. *)

  val compile_args :
       args:string list
    -> emits:Id.t
    -> infile:string
    -> outfile:string
    -> string list
  (** [compile_args ~args ~emits ~infile ~outfile] takes the set of
      arguments [args] the user supplied, the name of the architecture
      [emits] the compiler is going to emit, the input file [infile], and
      the output file [outfile], and produces a final argument vector to
      send to the compiler. *)
end

(** [S] is the outward-facing interface of compiler modules. *)
module type S = sig
  val test : unit -> unit Or_error.t
  (** [test ()] tests that the compiler is working. *)

  val compile : infile:Fpath.t -> outfile:Fpath.t -> unit Or_error.t
  (** [compile ~infile ~outfile] runs the compiler on [infile], emitting
      assembly to [outfile] and returning any errors that arise. *)
end

(** {2 Resolving spec IDs to compilers}

    These signatures describe modules that allow one to retrieve
    (first-class) compiler modules at run-time from compiler specifications. *)

(** Basic input type of resolvers. *)
module type Basic_resolver = sig
  (** Type of specifications consumed by this resolver. *)
  type spec

  val resolve : spec -> (module Basic) Or_error.t
end

(** Signature of fully-instantiated resolvers. *)
module type S_resolver = sig
  (** Type of specifications consumed by this resolver. *)
  type spec

  (** Type of chained-filter input. *)
  type 'a chain_input

  val from_spec : spec -> (module S) Or_error.t
  (** [from_spec spec] attempts to produce a first-class compiler module
      corresponding to [spec]. *)

  val filter_from_spec :
       spec
    -> (module Plumbing.Filter_types.S
          with type aux_i = unit
           and type aux_o = unit)
       Or_error.t
  (** [filter_from_spec spec] attempts to produce a first-class compiler
      filter corresponding to [spec]. *)

  val chained_filter_from_spec :
       spec
    -> (module Plumbing.Filter_types.S
          with type aux_i = 'i
           and type aux_o = 'o)
    -> (module Plumbing.Filter_types.S
          with type aux_i = 'i chain_input
           and type aux_o = 'o)
       Or_error.t
end
