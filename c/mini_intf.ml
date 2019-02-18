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

open Core_kernel

(** {2 Paths} *)

(** {3 Phantom types for path GADTs} *)

type on_expr
type on_stm
type stm_hole

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

(** Signature of paths over statements *)
module type S_statement_path = sig
  type stm
  type 'a list_path

  type 'a t =
    | This : on_stm t
    | If_block : { branch : bool ; rest : 'a list_path } -> 'a t
    | If_cond : on_expr t
  ;;

  include S_path with type 'a t := 'a t
                  and type target := stm
                  and type stm := stm

  val try_gen_insert_stm
    : stm -> stm_hole t Quickcheck.Generator.t option
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
  type 'a stm_path

  type 'a t =
    | Insert_at : int -> stm_hole t
    | At        : { index : int; rest : 'a stm_path } -> 'a t

  include S_stm_container_path with type 'a t := 'a t
                                and type target := stm list
                                and type stm := stm
end

(** Signature of paths over functions *)
module type S_function_path = sig
  type 'a stm_list_path

  type 'a t =
    | On_statements : 'a stm_list_path -> 'a t
  ;;

  include S_stm_container_path with type 'a t := 'a t
end

(** Signature of paths over programs *)
module type S_program_path = sig
  type 'a function_path

  type 'a t =
    | On_program : { index : int; rest : 'a function_path } -> 'a t
  ;;

  include S_stm_container_path with type 'a t := 'a t
end
