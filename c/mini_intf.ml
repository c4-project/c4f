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

(** Mini-model: module signatures *)

open Core_kernel

(** Signature of parts of the mini-model that implement type checking. *)
module type S_type_checkable = sig
  type t
  (** The type being checked. *)

  module Type_check (E : Mini_env.S) : sig
    val type_of : t -> Mini_type.t Or_error.t
    (** [type_of x] tries to get the type of [x] given the variable
        typing environment [E.env].  It fails if the type is
        inconsistent. *)
  end
end

(** {2 Paths} *)

(** {3 Phantom types for path GADTs} *)

type on_expr
(** Marks a path as reaching an existing expression. *)

type on_stm
(** Marks a path as reaching an existing statement. *)

type stm_hole
(** Marks a path as reaching a space where we can insert a statement. *)

(** {3 Path GADTs} *)

type 'a stm_path =
  | This     : on_stm stm_path
  | If_block : { branch : bool ; rest : 'a list_path } -> 'a stm_path
  | If_cond  : on_expr stm_path
and 'a list_path =
  | Insert_at : int -> 'a list_path
  | At        : { index : int; rest : 'a stm_path } -> 'a list_path
;;

type 'a function_path =
  | On_statements : 'a list_path -> 'a function_path
;;

type 'a program_path =
  | On_program : { index : int; rest : 'a function_path } -> 'a program_path
;;

(** {3 Signatures} *)

(** General signature of paths *)
module type S_path = sig
  type 'a t
  type target
  type stm

  val insert_stm : stm_hole t -> stm -> target -> target Or_error.t
  (** [insert_stm path stm dest] tries to insert [stm] into the part
     of [dest] pointed to by [path]. *)
end

(** Signature of paths over statements and statement-like entities. *)
module type S_statement_path = sig
  type stm
  type target

  include S_path with type 'a t := 'a stm_path
                  and type target := target
                  and type stm := stm

  val lift_stm : stm -> target
  (** [lift_stm s] lifts a generated statement [s] to the target type
     of this path. *)

  val lower_stm : target -> stm
  (** [lower_stm s] lowers a generated statement [s] to the statement
     type of this path. *)

  val try_gen_insert_stm
    : target -> stm_hole stm_path Quickcheck.Generator.t option
    (** [try_gen_insert_stm dest] tries to create a Quickcheck-style
       generator for statement insertion paths targeting [dest].

        It can return [None] if [dest] has no position at which
       statements can be inserted. *)
end

module type S_stm_container_path = sig
  include S_path

  val gen_insert_stm : target -> stm_hole t Quickcheck.Generator.t
  (** [gen_insert_stm dest] creates a Quickcheck-style
        generator for statement insertion paths targeting [dest]. *)
end

(** Signature of paths over statement lists *)
module type S_statement_list_path = sig
  type stm
  type target

  include S_stm_container_path with type 'a t := 'a list_path
                                and type target := target list
                                and type stm := stm
end

(** Signature of paths over functions *)
module type S_function_path = sig
  include S_stm_container_path with type 'a t := 'a function_path
end

(** Signature of paths over programs *)
module type S_program_path = sig
  include S_stm_container_path with type 'a t := 'a program_path
end
