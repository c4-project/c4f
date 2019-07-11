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

(** Mini-model: path-based traversal *)

open Base

(** {2 Phantom types for path GADTs} *)

type on_expr [@@deriving sexp]
(** Marks a path as reaching an existing expression. *)

type on_stm [@@deriving sexp]
(** Marks a path as reaching an existing statement. *)

type stm_hole [@@deriving sexp]
(** Marks a path as reaching a space where we can insert a statement. *)

(** {2 Path GADTs} *)

type 'a stm_path =
  | In_if : 'a if_path -> 'a stm_path
  | This : on_stm stm_path
[@@deriving sexp_of]

and 'a list_path =
  | Insert_at : int -> 'a list_path
  | At : {index: int; rest: 'a stm_path} -> 'a list_path
[@@deriving sexp_of]

and 'a if_path =
  | Block : {branch: bool; rest: 'a list_path} -> 'a if_path
  | Cond : on_expr if_path
[@@deriving sexp_of]

type 'a function_path = On_statements : 'a list_path -> 'a function_path
[@@deriving sexp_of]

type 'a program_path =
  | On_program : {index: int; rest: 'a function_path} -> 'a program_path
[@@deriving sexp_of]

(** {2 Signatures}

    We don't keep these in a separate MLI file, because they depend directly
    and intricately on the types defined above. *)

(** General signature of paths. *)
module type S_path = sig
  type 'a t

  type target

  val insert_stm : stm_hole t -> Statement.t -> target -> target Or_error.t
  (** [insert_stm path stm dest] tries to insert [stm] into the part of
      [dest] pointed to by [path]. *)

  val transform_stm :
       on_stm t
    -> f:(Statement.t -> Statement.t Or_error.t)
    -> target
    -> target Or_error.t
  (** [transform_stm path ~f dest] tries to modify the statement at [stm]
      using [f]. *)
end

(** Signature of paths over statements and statement-like entities. *)
module type S_statement = sig
  type target

  include S_path with type 'a t := 'a stm_path and type target := target

  val lift_stm : Statement.t -> target
  (** [lift_stm s] lifts a generated statement [s] to the target type of
      this path. *)

  val lower_stm : target -> Statement.t
  (** [lower_stm s] lowers a generated statement [s] to the statement type
      of this path. *)

  val try_gen_insert_stm :
    target -> stm_hole stm_path Base_quickcheck.Generator.t option
  (** [try_gen_insert_stm dest] tries to create a Quickcheck-style generator
      for statement insertion paths targeting [dest].

      It can return [None] if [dest] has no position at which statements can
      be inserted. *)
end

module type S_stm_container = sig
  include S_path

  val gen_insert_stm : target -> stm_hole t Base_quickcheck.Generator.t
  (** [gen_insert_stm dest] creates a Quickcheck-style generator for
      statement insertion paths targeting [dest]. *)
end

(** Signature of paths over conditionals *)
module type S_if_statement = sig
  type target

  include S_path with type 'a t := 'a if_path and type target := target

  include
    S_stm_container with type 'a t := 'a if_path and type target := target
end

(** Signature of paths over statement lists *)
module type S_statement_list = sig
  type target

  include
    S_stm_container
      with type 'a t := 'a list_path
       and type target := target list
end

(** Signature of paths over functions *)
module type S_function = sig
  include S_stm_container with type 'a t := 'a function_path
end

(** Signature of paths over programs *)
module type S_program = sig
  include S_stm_container with type 'a t := 'a program_path
end

(** {2 Functors} *)

module Make_statement_list (M : S_statement) :
  S_statement_list with type target = M.target

module Statement_list : S_statement_list with type target = Statement.t

module If_statement : S_if_statement with type target = Statement.If.t

module Statement : S_statement with type target = Statement.t

module Function : S_function with type target := Function.t

module Program : S_program with type target := Program.t
