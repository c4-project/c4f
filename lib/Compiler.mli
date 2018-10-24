(* This file is part of 'act'.

Copyright (c) 2018 by Matt Windsor

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. *)

(** [Compiler] contains the high-level interface for specifying and
   invoking compilers. *)

open Core
open Utils

(*
 * Specifications: parts common to both compilers and machines
 *)

(** [Id] is a module for compiler and machine identifiers. *)
module Id : sig
  (** [t] is the type of compiler IDs. *)
  type t

  (** [to_string_list cid] returns a list of each element in [cid]'s
     ID. *)
  val to_string_list : t -> string list

  include Core.Identifiable.S with type t := t
end

(** [SpecS] is the basic interface of both compiler and
    machine specifications. *)
module type SpecS = sig
  (** [t] is the opaque type of specifications.
      To construct a [t], read one in as an S-expression;
      a proper constructor may appear in later revisions. *)
  type t

  (** [enabled c] gets whether [c] is enabled. *)
  val enabled : t -> bool

  include Pretty_printer.S with type t := t
  include Sexpable.S with type t := t

  (** [pp_summary f spec] prints a one-line summary of [spec]. *)
  val pp_summary : Format.formatter -> t -> unit
end

(** [SpecIntf] is the top-level, outward-facing interface of both
   compiler and machine specifications. *)
module type SpecIntf = sig
  include SpecS

  (** [WithId] contains types and functions for handling bundles of
     spec ID and spec. *)
  module WithId : sig
    type elt = t

    (** [t] is the type of ID-and-spec pairs. *)
    type t

    (** [create ~id ~spec] creates a new [WithId.t] pair. *)
    val create : id:Id.t -> spec:elt -> t;;

    (** [id w] gets the ID component of a [WithId.t]. *)
    val id : t -> Id.t;;

    (** [spec w] gets the spec component of a [WithId.t]. *)
    val spec : t -> elt;;

    include Sexpable.S with type t := t
  end

  (** [Set] is the interface of modules for dealing with sets of
      compiler specs. *)
  module Set : sig
    type elt = t

    (** [t] is the type of sets. *)
    type t

    include Pretty_printer.S with type t := t
    include Sexpable.S with type t := t

    (** [pp_verbose verbose f specs] prints a [specs] with the level of
        verbosity implied by [verbose]. *)
    val pp_verbose : bool -> Format.formatter -> t -> unit

    (** [get specs id] tries to look up ID [id] in [specs],
        and emits an error if it can't. *)
    val get : t -> Id.t -> elt Or_error.t

    (** [of_list xs] tries to make a set from [xs].
        It raises an error if [xs] contains duplicate IDs. *)
    val of_list : WithId.t list -> t Or_error.t

    (** [partition_map specs ~f] applies a partitioning predicate [f] to the
        specifications in [specs], returning those marked [`Fst] in
        the first bucket and those marked [`Snd] in the second. *)
    val partition_map
      :  t
      -> f : (WithId.t -> [`Fst of 'a | `Snd of 'b])
      -> ('a list * 'b list)
    ;;

    (** [map specs ~f] applies a mapper [f] to the
        specifications in [specs], returning the results as a list. *)
    val map
      :  t
      -> f : (WithId.t -> 'a)
      -> 'a list
    ;;
  end

  (** [pp_verbose verbose f spec] prints a [spec] with the level of
      verbosity implied by [verbose]. *)
  val pp_verbose : bool -> Format.formatter -> t -> unit
end

(** [MakeSpec] makes a [SpecIntf] from a [SpecS]. *)
module MakeSpec
  : functor (S : SpecS)
    -> SpecIntf with type t = S.t
;;

(*
 * Machines
 *)

(** [MRefIntf] is the signature any machine reference module
    going into [MakeCSpec] must implement. *)
module type MRefIntf = sig
  (** [t] is the type of machine references. *)
  type t

  include Pretty_printer.S with type t := t
  include Sexpable.S with type t := t

  (** [default] is the default machine reference, used whenever a
      reference is omitted in a compiler specification. *)
  val default : t;;

  (** [is_remote m] returns a guess as to whether machine reference
     [m] is a reference to a remote machine. *)
  val is_remote : t -> bool;;
end

(** [SshIntf] is the interface of modules defining SSH
    configuration. *)
module type SshIntf = sig
  type t

  include Pretty_printer.S with type t := t
  include Sexpable.S with type t := t

  (** [host] gets the hostname of the SSH remote. *)
  val host : t -> string
  (** [user] gets the optional username of the SSH remote. *)
  val user : t -> string option
  (** [copy_dir] gets the remote directory to which we'll be
      copying work. *)
  val copy_dir : t -> string
end

(** [Ssh] is a module defining SSH configuration. *)
module Ssh : SshIntf

(** [MSpecIntf] is a the interface of modules defining machine
   specification types. *)
module type MSpecIntf = sig
  (** [t] describes a machine. *)
  type t;;

  (** [via] describes the connection to the machine. *)
  type via =
    | Local
    | Ssh of Ssh.t
  ;;
  (** [via spec] gets the [via] stanza of a machine spec [spec]. *)
  val via : t -> via;;

  (** Machine specifications are machine references... *)
  include MRefIntf with type t := t
  (** ...and specifications. *)
  include SpecIntf with type t := t
end

(** [MSpec] is a module for machine specifications. *)
module MSpec : MSpecIntf

(*
 * Compiler specifications
 *)

(** [CSpecIntf] is the interface of modules defining compiler
    specification types. *)
module type CSpecIntf = sig
  (** [Mach] is some module for resolving references to the machine on
     which a compiler is located.  This can either be an inline
     machine specification module, or a more indirect form of
     reference. *)
  module Mach : MRefIntf

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

  (** [herd] gets the optional Herd command to run on [c]'s output. *)
  val herd : t -> string option

  (** [machine] gets the machine reference for [c]. *)
  val machine : t -> Mach.t

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
    -> herd    : string option
    -> machine : Mach.t
    -> t
  ;;

  (** Compiler specifications are specifications! *)
  include SpecIntf with type t := t
end

(** [MakeCSpec] is a functor for building compiler specifications
   parametrised on machine references.

    See [CfgSpec] and [Spec] for implementations of this functor. *)
module MakeCSpec
  : functor (M : MRefIntf)
    -> CSpecIntf with module Mach = M
;;

(** [CfgCSpec] is a module describing compiler specs where machine
    references are unresolved IDs.  This is the format
    used in the configuration file, hence the name. *)
module CfgCSpec : CSpecIntf with type Mach.t = Id.t

(** [CSpec] is a module describing compiler specs where machine
    references are inlined [MSpec.t] records. *)
module CSpec : CSpecIntf with type Mach.t = MSpec.t

(** [WithCSpec] is an interface for modules containing a compiler
    specification. *)
module type WithCSpec = sig
  val cspec : CSpec.WithId.t
end

(*
 * Running compilers
 *)

(** [S] is the basic interface compilers must implement. *)
module type S = sig
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
  val pre : infile:string -> outfile:string -> (string * string) Or_error.t

  (** [post ~infile ~outfile] is run after a successful compile.
      It receives the *original* input and output file names---not the
      ones returned by [pre]. *)
  val post : infile:string -> outfile:string -> unit Or_error.t
end

(** [Intf] is the outward-facing interface of compiler modules. *)
module type Intf = sig
  include WithCSpec

  (** [test ()] tests that the compiler is working. *)
  val test : unit -> unit Or_error.t

  (** [compile ~infile ~outfile] runs the compiler on [infile],
      emitting assembly to [outfile] and returning any errors that arise. *)
  val compile : infile:string -> outfile:string -> unit Or_error.t
end

(** [Make] produces a runnable compiler satisfying [Intf] from a
    triple of spec [P], compiler [C], hooks [H], and runner [R]. *)
module Make
  : functor (P : WithCSpec)
    -> functor (C : S)
      -> functor (H : Hooks)
        -> functor (R : Run.Runner)
          -> Intf

(** [from_spec f spec] takes a function that generates a first-class
   [S] from a spec, and attempts to produce a first-class compiler
   module. *)
val from_spec
  :  (CSpec.WithId.t -> (module S) Or_error.t)
  -> CSpec.WithId.t
  -> (module Intf) Or_error.t
