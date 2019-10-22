(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

type index = int [@@deriving sexp]

type length = int [@@deriving sexp]

type branch = bool [@@deriving sexp]

type stm = In_if of ifs | This_stm [@@deriving sexp]

and stm_list =
  | Insert of index
  | In_stm of index * stm
  | On_stm_range of index * length
[@@deriving sexp]

and ifs = In_block of branch * stm_list | This_cond [@@deriving sexp]

let in_if (rest : ifs) : stm = In_if rest

let this_stm : stm = This_stm

let insert (i : index) : stm_list = Insert i

let in_stm (i : index) (rest : stm) : stm_list = In_stm (i, rest)

let in_block (b : branch) (rest : stm_list) : ifs = In_block (b, rest)

let this_cond : ifs = This_cond

type func = In_stms of stm_list [@@deriving sexp]

let in_stms (rest : stm_list) : func = In_stms rest

type program = In_func of index * func [@@deriving sexp]

let in_func (i : index) (rest : func) : program = In_func (i, rest)
