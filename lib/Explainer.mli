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

open Core
open Lang

module type S = sig
  type statement

  (** [stm_explanation] is the type of statement explanations. *)
  type stm_explanation =
    { original : statement
    ; abs_type : Language.abs_statement
    ; flags : Language.FlagSet.t
    }

  (** [t] is the type of assembly explanations. *)
  type t =
    { statements : stm_explanation list
    }

  (** We can [pp] explanations. *)
  include Pretty_printer.S with type t := t

  (** [explain lst] compiles a [t] for assembly listing [lst]. *)
  val explain : statement list -> t
end

(** [Make] makes an implementation of [S] for a given language. *)
module Make : functor (LS : Language.Intf) -> (S with type statement = LS.Statement.t)
