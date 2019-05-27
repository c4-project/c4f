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

(** [Compiler] contains the high-level interface for specifying and invoking
    compilers. *)

open Core_kernel
open Act_common
open Compiler_intf

(** [Make_spec] is a functor for building compiler specifications
    parametrised on machine references.

    See [Cfg_spec] and [Spec] for implementations of this functor. *)
module Make_spec (M : Machine.Reference) : S_spec with module Mach = M

(** [Cfg_spec] is a module describing compiler specs where machine
    references are unresolved IDs. This is the format used in the
    configuration file, hence the name. *)
module Cfg_spec : S_spec with type Mach.t = Id.t

(** [Spec] is a module describing compiler specs where machine references
    are inlined machine specs. This is the form of compiler spec expected
    through most of act. *)
module Spec : S_spec with type Mach.t = Machine.Spec.With_id.t

(** Type of target specifiers for jobs that can accept both C files and
    assembly. *)
module Target : sig
  (** [t] is either a compiler specifier, or a raw architecture. *)
  type t = [`Spec of Spec.With_id.t | `Arch of Id.t]

  val arch : t -> Id.t
  (** [arch_of_target target] gets the architecture ID associated with
      [target]. *)

  val ensure_spec : t -> Spec.With_id.t Or_error.t
  (** [ensure_spec target] extracts a compiler spec from [target], failing
      if it is a raw architecture. *)
end

(** A compiler that always passes its tests, but fails with [E.error] on
    runtime.

    Generally used when building a compiler chain fails, but the error is
    one that can be sidestepped over if the compiler never gets run. *)
module Fail (E : sig
  val error : Error.t
end) : S

(** [Property] contains a mini-language for querying compiler specs,
    suitable for use in [Blang]. *)
module Property : sig
  (** [t] is the opaque type of property queries. *)
  type t [@@deriving sexp]

  val id : Id.Property.t -> t
  (** [id] constructs a query over a compiler's ID. *)

  val machine : Machine.Property.t -> t
  (** [machine] constructs a query over a compiler's machine. *)

  val eval : Spec.With_id.t -> t -> bool
  (** [eval cspec property] evaluates [property] over [cspec]. *)

  val eval_b : Spec.With_id.t -> t Blang.t -> bool
  (** [eval_b cspec expr] evaluates a [Blang] expression [expr] over
      [cspec]. *)

  include Property.S with type t := t
end

(** [With_spec] is an interface for modules containing a (full) compiler
    specification. *)
module type With_spec = sig
  val cspec : Spec.With_id.t
end

(** [Basic_with_run_info] is a signature collecting both a base compiler
    specification and context about how to run the compiler. *)
module type Basic_with_run_info = sig
  include Basic

  include With_spec

  module Runner : Plumbing.Runner_types.S
end

(** [Make] produces a runnable compiler satisfying [S] from a
    [Basic_with_run_info]. *)
module Make (B : Basic_with_run_info) : S

(** {2 Filters}

    These functors and functions lift compilers into the
    {{!Utils.Filter} filter} system, so that they can be composed with other
    similar passes. *)

(** Lifts a [S] to a filter. *)
module S_to_filter (S : S) :
  Plumbing.Filter_types.S with type aux_i = unit and type aux_o = unit

(** Shorthand for [Make_filter (S_to_filter (B))]. *)
module Make_filter (B : Basic_with_run_info) :
  Plumbing.Filter_types.S with type aux_i = unit and type aux_o = unit

(** Abstract type of auxiliary input wrappers used for compiler chains. *)
module Chain_input : sig
  type next_mode = [`Preview | `No_compile | `Compile]

  type 'a t

  val file_type : 'a t -> File_type.t

  val next : 'a t -> next_mode -> 'a

  val make : file_type:File_type.t -> next:(next_mode -> 'a) -> 'a t
end

(** Version of {{!Compiler_intf.S_resolver} Compiler_intf.S_resolver} with
    the chain input fixed. *)
module type S_resolver =
  S_resolver with type 'a chain_input := 'a Chain_input.t

(** {2 Resolving spec IDs to compilers} *)

(** Constructs a {{!S_resolver} S_resolver} from a
    {{!Basic_resolver} Basic_resolver} over direct compiler specs. *)
module Make_resolver (B : Basic_resolver with type spec := Spec.With_id.t) :
  S_resolver with type spec = Spec.With_id.t

(** Constructs a {{!S_resolver} S_resolver} over targets from a
    {{!Basic_resolver} Basic_resolver}. *)
module Make_target_resolver
    (B : Basic_resolver with type spec := Spec.With_id.t) :
  S_resolver with type spec = Target.t
