(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

type stm = In_if of ifs | This_stm [@@deriving sexp]

and stm_list = Insert of {index: int} | In_stm of {index: int; rest: stm}
[@@deriving sexp]

and ifs = In_block of {branch: bool; rest: stm_list} | This_cond
[@@deriving sexp]

let in_if (rest : ifs) : stm = In_if rest

let this_stm : stm = This_stm

let insert (index : int) : stm_list = Insert {index}

let in_stm (index : int) (rest : stm) : stm_list = In_stm {index; rest}

let in_block (branch : bool) (rest : stm_list) : ifs =
  In_block {branch; rest}

let this_cond : ifs = This_cond

type func = In_stms of stm_list [@@deriving sexp]

let in_stms (rest : stm_list) : func = In_stms rest

type program = In_func of {index: int; rest: func} [@@deriving sexp]

let in_func (index : int) (rest : func) : program = In_func {index; rest}
