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

(** Fuzzer configuration.

    This module contains the top-level configuration for `act`'s litmus test
    mutator. *)

open Base
open Act_common

type t [@@deriving sexp]
(** Opaque type of fuzzer configurations. *)

(** {2 Constructors} *)

val make : ?weights:(Id.t, int) List.Assoc.t -> unit -> t
(** [make ?weights ()] constructs a fuzzer configuration with the given
    action weights table (defaulting to empty). *)

val of_ast : Ast.Fuzz.t list -> t Or_error.t
(** [of_ast ast] interprets a fuzzer configuration block [ast]. It returns
    the resulting block if well-formed, and an error otherwise. *)

(** {2 Accessors} *)

val weights : t -> (Id.t, int) List.Assoc.t
(** [weights t] gets the action weight table of [config] *)
