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

(** [Compiler] contains the high-level interface for specifying and
    invoking compilers. *)

open Core_kernel
open Utils

(** To reduce duplication, we describe the module types of [Compiler] in
    [Compiler_intf], and import parts of it both here and in the
    implementation. *)
include module type of Compiler_intf

module Make_spec (M : Machine.Reference) : S_spec with module Mach = M
(** [Make_spec] is a functor for building compiler specifications
    parametrised on machine references.

    See [Cfg_spec] and [Spec] for implementations of this
    functor. *)

module Cfg_spec : S_spec with type Mach.t = Id.t
(** [Cfg_spec] is a module describing compiler specs where machine
    references are unresolved IDs.  This is the format
    used in the configuration file, hence the name. *)

module Spec : S_spec with type Mach.t = Machine.Spec.With_id.t
(** [Spec] is a module describing compiler specs where machine
    references are inlined machine specs.  This is the
    form of compiler spec expected through most of act. *)

(** [Property] contains a mini-language for querying compiler specs,
    suitable for use in [Blang]. *)
module Property : sig
  type t [@@deriving sexp]
  (** [t] is the opaque type of property queries. *)

  val id : Id.Property.t -> t
  (** [id] constructs a query over a compiler's ID. *)

  val machine : Machine.Property.t -> t
  (** [machine] constructs a query over a compiler's machine. *)

  val eval : Spec.With_id.t -> t -> bool
  (** [eval cspec property] evaluates [property] over [cspec]. *)

  (** [eval_b cspec expr] evaluates a [Blang] expression [expr] over
      [cspec]. *)
  val eval_b : Spec.With_id.t -> t Blang.t -> bool

  include Property.S with type t := t
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
  module Runner : Runner.S
end

module Make (B : Basic_with_run_info) : S
(** [Make] produces a runnable compiler satisfying [S] from a
    [Basic_with_run_info]. *)

val from_spec
  :  (Spec.With_id.t -> (module Basic) Or_error.t)
  -> Spec.With_id.t
  -> (module S) Or_error.t
(** [from_spec f spec] takes a function that generates a first-class
    [Basic] from a spec, and attempts to produce a first-class
    compiler module. *)
