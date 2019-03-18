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

open Base
open Utils

(** [Reference] is the signature of references to machines. *)
module type Reference = sig
  (** [t] is the type of machine references. *)
  type t [@@deriving sexp]

  include Pretty_printer.S with type t := t

  (** [default] is the default machine reference, used whenever a
      reference is omitted in a compiler specification. *)
  val default : t

  (** [id m] gets the machine ID of this reference. *)
  val id : t -> Id.t

  (** [remoteness m] returns a guess as to whether machine reference
      [m] is a reference to a remote machine. *)
  val remoteness : t -> [`Remote | `Local | `Unknown]
end

(** [Basic_spec] is the signature common to any sort of machine
    specification, including [With_id] pairs.

    In practice, modules implementing this will either be [Spec]
    or [Spec.With_id]. *)
module type Basic_spec = sig
  type t
  (** [t] describes a machine. *)

  type via
  (** Type of 'via' blocks. *)

  val via : t -> via
  (** [via spec] gets the [via] stanza of a machine spec [spec]. *)

  val litmus : t -> Litmus_tool.t option
  (** [litmus spec] gets any available configuration in [spec] for
      the Litmus tool. *)

  val ensure_litmus : t -> Litmus_tool.t Or_error.t
  (** [ensure_litmus spec] behaves as [litmus spec], but returns a
      descriptive error if the Litmus configuration is missing. *)

  val runner : t -> (module Runner.S)
  (** [to_runner spec] gets a runner for the machine spec
     [spec]. *)

end
