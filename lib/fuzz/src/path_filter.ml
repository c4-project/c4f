(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

type t =
  { final_stm: (Subject.Statement.t -> bool) list
  ; thru_block: (Subject.Block.t -> bool) list }

let empty : t = {final_stm= []; thru_block= []}

let add_final_stm (existing : t) ~(f : Subject.Statement.t -> bool) : t =
  {existing with final_stm= f :: existing.final_stm}

let final_if_statements_only : t -> t =
  add_final_stm ~f:Act_c_mini.Statement.is_if_statement

let is_final_statement_ok (filter : t) ~(stm : Subject.Statement.t) : bool =
  List.for_all filter.final_stm ~f:(fun x -> x stm)
