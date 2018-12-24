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

(** Top-level AST for Litmus files *)

open Core

(** Signature containing just the parts of an act language needed
    for building Litmus file ASTs. *)
module type Basic = sig
  val name : string

  (** Abstract type of constant syntax *)
  module Constant : sig
    type t [@@deriving sexp]
    include Pretty_printer.S with type t := t
  end

  (** Abstract type of statement syntax *)
  module Statement : sig
    type t [@@deriving sexp]
    include Pretty_printer.S with type t := t
  end
end

(** [S] is the interface for litmus AST modules. *)
module type S = sig
  module Lang : Basic

  type t =
    { name : string
    ; init : ((string, Lang.Constant.t) List.Assoc.t) (* Initial heap *)
    ; programs : Lang.Statement.t list list
    }

  include Pretty_printer.S with type t := t

  (** [pp_programs f t] pretty-prints [t]'s program (only) to [f]. *)
  val pp_programs : Format.formatter -> t -> unit

  (** [make] tries to build a [t] from a name [name], initialiser
      [init], and program list [programs].  It returns a result,
      as it may fail if the input isn't a valid Litmus program. *)
  val make
    :  name:string
    -> init:((string, Lang.Constant.t) List.Assoc.t)
    -> programs:Lang.Statement.t list list
    -> t Or_error.t
  ;;
end

module Make (Lang : Basic) : S with module Lang = Lang
(** [Make] is a functor that, given a language described by
    [Basic], produces a module type for litmus test syntax
    trees, as well as operations for pretty-printing it. *)
