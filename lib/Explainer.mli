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

(** High-level module for emitting act's internal assembly analysis

[Explainer] contains functors for extracting a pretty-printable
   summary of an assembly listing as act understands it, through a
   language module.  *)

open Core_kernel

(** [Basic_explanation] is the signature that must be implemented by any
    module going into [Make_explanation]. *)
module type Basic_explanation = sig
  (** [elt] is the type of the concrete item being explained. *)
  type elt
  (** [context] is the type of context that must be supplied with
      an item ([elt]) to compute an explanation. *)
  type context

  (** Items of type [elt] must be abstractable. *)
  include Abstractable.S with type t := elt

  (** Items of type [elt] must also be printable. *)
  include Pretty_printer.S with type t := elt

  (** [Flag] is the module containing detail flags that may be raised
     on [elt]. *)
  module Flag : Abstract_flag.S

  (** [abs_flags elt context] computes the set of detail flags that
     apply to [elt] given the context [context]. *)
  val abs_flags : elt -> context -> Flag.Set.t
end

(** [Explanation] modules contain an abstract data type [t] that encapsulates
    a concrete but [Abstractable] type [elt] alongside a pre-computed summary
    of its abstract analysis with respect to a given context of type
    [context]. *)
module type Explanation = sig
  (** [t] is the opaque type of the explanation itself. *)
  type t
  (** [elt] is the type of the concrete item being explained. *)
  type elt
  (** [context] is the type of context that must be supplied with
      an item ([elt]) to compute an explanation. *)
  type context

  (** Each [t] inherits the abstract type projection of its underlying [exp]. *)
  include Abstractable.S with type t := t

  (** Explanations can be pretty-printed. *)
  include Pretty_printer.S with type t := t

  (** [Flag] is the module containing detail flags that may be raised
     on the explanation's original element. *)
  module Flag : Abstract_flag.S

  (** [original exp] gets the original element of an explanation [exp]. *)
  val original : t -> elt

  (** [abs_flags exp] gets detailed analysis of the element inside [exp] in
      the form of the abstract representation's flag set. *)
  val abs_flags : t -> Flag.Set.t

  (** [create ~context ~original] creates a [t] over [original], using
      [context] to get more detailed information than would normally be
      available through [abs_type] alone. *)
  val create
    :  context:context
    -> original:elt
    -> t
end

(** [Make_explanation (B)] makes an [Explanation] from a [Basic_explanation]. *)
module Make_explanation
  : functor (B : Basic_explanation)
    -> Explanation with type elt := B.elt
                    and type context := B.context
                    and module Abs := B.Abs
                    and module Flag := B.Flag
;;

module type S = sig
  type statement

(** [Stm_explanation] provides explanations for [statement]s, given symbol
    tables as context. *)
  module Stm_explanation
    : Explanation with type elt := statement
                   and type context := Abstract.Symbol.Table.t
                   and module Abs := Abstract.Statement
  ;;

  (** [t] is the type of whole-program explanations. *)
  type t =
    { statements : Stm_explanation.t list
    }

  (** We can [pp] explanations. *)
  include Pretty_printer.S with type t := t
  (** [pp_as_assembly f exp] prints [exp] in a smaller summary format that
      (ideally) parses as valid assembly. *)
  val pp_as_assembly : Base.Formatter.t -> t -> unit

  (** [explain lst] compiles a [t] for assembly listing [lst]. *)
  val explain : statement list -> t
end

(** [Make] makes an implementation of [S] for a given language. *)
module Make
  : functor (LS : Language.S)
    -> S with type statement := LS.Statement.t
;;
