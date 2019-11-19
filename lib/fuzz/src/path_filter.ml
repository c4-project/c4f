(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

type t =
  { on_end: (t -> bool) list
  ; on_final_stm: (Subject.Statement.t -> bool) list
  ; in_dead_code: bool }

let empty : t = {on_end= []; on_final_stm= []; in_dead_code= false}

let add_on_end (existing : t) ~(f : t -> bool) : t =
  {existing with on_end= f :: existing.on_end}

let add_on_final_stm (existing : t) ~(f : Subject.Statement.t -> bool) : t =
  {existing with on_final_stm= f :: existing.on_final_stm}

let in_dead_code_only : t -> t = add_on_end ~f:(fun x -> x.in_dead_code)

let final_if_statements_only : t -> t =
  add_on_final_stm ~f:Act_c_mini.Statement.is_if_statement

let update_with_block_metadata (existing : t) (m : Metadata.t) : t =
  { existing with
    in_dead_code= existing.in_dead_code || Metadata.is_dead_code m }

let is_ok (filter : t) : bool =
  List.for_all filter.on_end ~f:(fun x -> x filter)

let is_final_statement_ok (filter : t) ~(stm : Subject.Statement.t) : bool =
  is_ok filter && List.for_all filter.on_final_stm ~f:(fun x -> x stm)
