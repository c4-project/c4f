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

open Core
open Utils

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
  val emits : t -> string list

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
    -> emits   : string list
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
    -> emits   : string list
    -> infile  : string
    -> outfile : string
    -> string list
end

(** [Hooks] is the signature of mechanisms used to tell compilers to
    do something before/after a compilation.

    The main use of this module is to set up file transfers when doing
    remote compilation. *)
module type Hooks = sig
  (** [pre ~infile ~outfile] is run before compiling, and, if the
      hook is successful, returns the new infile and outfile to use
      for the compilation. *)
  val pre : infile:Fpath.t -> outfile:Fpath.t -> (string * string) Or_error.t

  (** [post ~infile ~outfile] is run after a successful compile.
      It receives the *original* input and output file names---not the
      ones returned by [pre]. *)
  val post : infile:Fpath.t -> outfile:Fpath.t -> unit Or_error.t
end

(** [S] is the outward-facing interface of compiler modules. *)
module type S = sig
  (** [test ()] tests that the compiler is working. *)
  val test : unit -> unit Or_error.t

  (** [compile ~infile ~outfile] runs the compiler on [infile],
      emitting assembly to [outfile] and returning any errors that arise. *)
  val compile : infile:Fpath.t -> outfile:Fpath.t -> unit Or_error.t
end

(** [Compiler] is the part of the interface re-exported as
    [Compiler.mli]. *)
module type Compiler = sig
  module type Basic_spec = Basic_spec
  module type S_spec = S_spec
  module type Basic = Basic
  module type Hooks = Hooks
  module type S = S

  (** [Make_spec] is a functor for building compiler specifications
      parametrised on machine references.

      See [Cfg_spec] and [Spec] for implementations of this
      functor. *)
  module Make_spec
    : functor (M : Machine.Reference)
      -> S_spec with module Mach = M
  ;;

  (** [Cfg_spec] is a module describing compiler specs where machine
      references are unresolved IDs.  This is the format
      used in the configuration file, hence the name. *)
  module Cfg_spec : S_spec with type Mach.t = Id.t

  (** [Spec] is a module describing compiler specs where machine
      references are inlined machine specs.  This is the
      form of compiler spec expected through most of act. *)
  module Spec : S_spec with type Mach.t = Machine.Spec.With_id.t

  (** [Property] contains a mini-language for querying compiler specs,
      suitable for use in [Blang]. *)
  module Property : sig
    (** [t] is the opaque type of property queries. *)
    type t [@@deriving sexp]

    (** [id] constructs a query over a compiler's ID. *)
    val id : Id.Property.t -> t

    (** [machine] constructs a query over a compiler's machine. *)
    val machine : Machine.Property.t -> t

    (** [eval cspec property] evaluates [property] over [cspec]. *)
    val eval
      :  Spec.With_id.t
      -> t
      -> bool
    ;;

    (** [eval_b cspec expr] evaluates a [Blang] expression [expr] over
       [cspec]. *)
    val eval_b
      :  Spec.With_id.t
      -> t Blang.t
      -> bool
    ;;
  end

  (** [With_spec] is an interface for modules containing a (full)
      compiler specification. *)
  module type With_spec = sig
    val cspec : Spec.With_id.t
  end

  (** [Basic_with_run_info] is a signature collecting both a base
      compiler specification and context about how to run the
      compiler. *)
  module type Basic_with_run_info = sig
    include Basic
    include With_spec
    module Runner : Run.Runner
    module Hooks : Hooks
  end

  (** [Make] produces a runnable compiler satisfying [S] from a
      [Basic_with_run_info]. *)
  module Make : functor (B : Basic_with_run_info) -> S

  (** [from_spec f spec] takes a function that generates a first-class
     [Basic] from a spec, and attempts to produce a first-class
     compiler module. *)
  val from_spec
    :  (Spec.With_id.t -> (module Basic) Or_error.t)
    -> Spec.With_id.t
    -> (module S) Or_error.t

end
