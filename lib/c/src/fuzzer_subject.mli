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

(** Fuzzer: subjects of fuzzing

    This module contains types for programs and litmus tests that are looser
    and lighter than their {{!Mini} Mini} versions, and more suited to
    mutation. *)

open Base

(** An item, annotated with information about its source. *)
module With_source : sig
  type 'a t

  val item : 'a t -> 'a
  (** [item x] gets [x]'s underlying item. *)

  val source : 'a t -> [`Existing | `Generated]
  (** [source x] gets [x]'s source. *)

  val make : item:'a -> source:[`Existing | `Generated] -> 'a t
  (** [make ~item ~source] makes a source-annotated item with contents
      [item] and source [source]. *)
end

(** Fuzzable representation of a program. *)
module Program : sig
  (** Transparent type of fuzzable programs. *)
  type t =
    { decls: Mini.Initialiser.t Mini_intf.id_assoc
    ; stms: Mini.Statement.t With_source.t list }
  [@@deriving sexp]

  (** Allows production and consumption of random paths over fuzzable
      programs in the same way as normal mini functions. *)
  module Path : Mini_path.S_function with type target := t

  val of_function : Mini.Function.t -> t
  (** [of_litmus func] converts a mini-model C function [func] to the
      intermediate form used for fuzzing. *)

  val to_function :
       t
    -> vars:Fuzzer_var.Map.t
    -> id:int
    -> Mini.Function.t Mini_intf.named Or_error.t
  (** [to_function prog ~vars ~id] lifts a subject-program [prog] with ID
      [prog_id] back into a Litmus function, adding a parameter list
      generated from [vars]. *)
end

(** Fuzzable representation of a litmus test. *)
module Test : sig
  (** Transparent type of fuzzable litmus tests. *)
  type t =
    {init: Mini.Constant.t Mini_intf.id_assoc; programs: Program.t list}
  [@@deriving sexp]

  val add_new_program : t -> t
  (** [add_new_program test] appends a new, empty program onto [test]'s
      programs list, returning the resulting test. *)

  (** Allows production and consumption of random paths over fuzzable tests
      in the same way as normal mini programs. *)
  module Path : Mini_path.S_program with type target := t

  val of_litmus : Mini_litmus.Ast.Validated.t -> t
  (** [of_litmus test] converts a validated C litmus test [test] to the
      intermediate form used for fuzzing. *)

  val to_litmus :
       ?postcondition:Mini_litmus.Ast.Postcondition.t
    -> t
    -> vars:Fuzzer_var.Map.t
    -> name:string
    -> Mini_litmus.Ast.Validated.t Or_error.t
  (** [to_litmus ?postcondition subject ~vars ~name] tries to reconstitute a
      validated C litmus test from the subject [subject], attaching the name
      [name] and optional postcondition [post], and using the variable map
      [vars] to reconstitute parameters. It may fail if the resulting litmus
      is invalid---generally, this signifies an internal error. *)

  (** {3 Helpers for mutating tests} *)

  val add_var_to_init : t -> Act_common.C_id.t -> Mini.Constant.t -> t
  (** [add_var_to_init subject var initial_value] adds [var] to [subject]'s
      init block with the initial value [initial_value]. *)
end
