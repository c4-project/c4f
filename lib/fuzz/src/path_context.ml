(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

type 'k t =
  { kind: 'k
  ; block_kind: Path_filter.Block.t
  ; block_len: int
  ; meta: Path_meta.t
  ; filter: Path_filter.t }
[@@deriving accessors]

let init ?(filter : Path_filter.t = Path_filter.zero) (kind : 'k) : 'k t =
  {kind; block_kind= Top; block_len= 0; meta= Path_meta.zero; filter}

let update_anchor (x : 'k t) ~(span : Utils.My_list.Span.t) : 'k t =
  let anchor = Path_meta.Anchor.of_dimensions ~span ~block_len:x.block_len in
  x.@(meta @> Path_meta.anchor) <- anchor

let add_flags (x : 'k t) (flags : Set.M(Path_meta.Flag).t) : 'k t Or_error.t
    =
  Or_error.Let_syntax.(
    (* TODO(@MattWindsor91): push meta further down. *)
    let meta = Path_meta.add_flags x.meta ~flags in
    let%map () = Path_filter.check_not x.filter ~meta in
    {x with meta})

let check_anchor (x : 'k t) : unit Or_error.t =
  Path_filter.check_anchor x.filter ?anchor:x.meta.anchor

let check_filter_req (x : 'k t) : unit Or_error.t =
  Or_error.all_unit
    [ Path_filter.check_req x.filter ~meta:x.meta
    ; Path_filter.check_block x.filter ~block:x.block_kind ]

let check_filter_stm (x : 'k t) ~(stm : Subject.Statement.t) :
    unit Or_error.t =
  Path_filter.check_final_statement x.filter ~stm

let check_filter_stms (x : 'k t) ~(stms : Subject.Statement.t list) :
    unit Or_error.t =
  Tx.Or_error.combine_map_unit stms ~f:(fun stm -> check_filter_stm x ~stm)

let check_end (x : 'k t) ~(stms : Subject.Statement.t list) : unit Or_error.t
    =
  Or_error.(
    all_unit
      [ tag (check_filter_req x) ~tag:"while checking flags"
      ; tag (check_filter_stms x ~stms) ~tag:"while checking statements"
      ; tag (check_anchor x) ~tag:"while checking anchor" ])

let check_thread_ok (x : _ t) ~(thread : int) : unit Or_error.t =
  Path_filter.check_thread_ok x.filter ~thread

let lift_path (x : 'k t) ~(path : 'p) : 'p Path_meta.With_meta.t =
  Path_meta.With_meta.make path ~meta:x.meta

(* TODO(@MattWindsor91): do something about this overload? *)
let kind (k : 'a t) : 'a = k.kind
