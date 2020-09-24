(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

(* This module used to be highly functionalised (with each check being a
   predicate function collected in a list); we replaced this with the present
   flag-based approach to give better error messages when paths fail
   validation. *)

module Block = struct
  type t =
    | Top
    | If of bool option
    | Flow of Fir.Statement_class.Flow.t option
  [@@deriving sexp, equal]

  (* All block constraints are disjoint, so if we merge together two path
     filters that disagree on them, we get an inconsistent check *)
  type chk = Valid of t | Inconsistent

  let merge (x : chk) (y : chk) : chk =
    match (x, y) with
    | Valid x, Valid y when equal x y ->
        Valid x
    | Valid _, Valid _
    | Valid _, Inconsistent
    | Inconsistent, Valid _
    | Inconsistent, Inconsistent ->
        Inconsistent

  let match_fail (block : t) ~(template : t) : unit Or_error.t =
    Or_error.error_s
      [%message
        "Final block of path doesn't match filter"
          ~block:(block : t)
          ~template:(template : t)]

  let match_req (block : t) ~(template : t) : unit Or_error.t =
    match (template, block) with
    | Top, Top | If None, If _ | Flow None, Flow _ ->
        Ok ()
    | If (Some x), If (Some y) when Bool.equal x y ->
        Ok ()
    | Flow (Some template), Flow (Some x)
      when Fir.Statement_class.Flow.class_matches x ~template ->
        Ok ()
    | _, _ ->
        match_fail block ~template

  let check (chk : chk) ~(block : t) : unit Or_error.t =
    match chk with
    | Inconsistent ->
        Or_error.error_string
          "path filter has inconsistent block requirements"
    | Valid template ->
        match_req block ~template
end

module Anchor = struct
  type t = Top | Bottom | Full [@@deriving sexp]

  let merge (l : t) (r : t) : t =
    match (l, r) with
    | Top, Top ->
        Top
    | Bottom, Bottom ->
        Bottom
    | _ ->
        Full

  module Check = struct
    type t = {is_nested: bool; pos: int; len: int; block_len: int}
    [@@deriving sexp]

    let of_path (path : Path.Stms.t) ~(block_len : int) : t =
      Path.Stms.
        { pos= path.@(index)
        ; len= len path
        ; is_nested= is_nested path
        ; block_len }
  end

  let is_anchored (anc : t) ~(check : Check.t) : bool =
    check.is_nested
    ||
    match anc with
    | Top ->
        check.pos = 0
    | Bottom ->
        check.block_len <= check.pos + check.len
    | Full ->
        check.pos = 0 && check.block_len <= check.len

  let check_anchor (anc : t) ~(path : Path.Stms.t) ~(block_len : int) :
      unit Or_error.t =
    let check = Check.of_path path ~block_len in
    Tx.Or_error.unless_m (is_anchored anc ~check) ~f:(fun () ->
        Or_error.error_s
          [%message
            "Path is not anchored properly"
              ~anchor:(anc : t)
              ~path_fragment:(path : Path.Stms.t)
              ~check:(check : Check.t)])
end

(** Checks that can only be carried out at the end of a statement path. *)
module End_check = struct
  module M = struct
    type t =
      | Stm_class of
          Act_fir.Class_constraint.t * Act_fir.Statement_class.t list
      | Stm_no_meta_restriction of Metadata.Restriction.t
      | Has_no_expressions_of_class of Act_fir.Expression_class.t list
    [@@deriving compare, equal, sexp]
  end

  include M
  include Comparable.Make (M)

  let is_ok (check : t) ~(stm : Subject.Statement.t) : bool =
    Act_fir.Statement_class.(
      match check with
      | Stm_class (req, templates) ->
          satisfies stm ~req ~templates
      | Stm_no_meta_restriction r ->
          not
            (Fir.Statement_traverse.On_meta.exists stm
               ~f:(Metadata.has_restriction r))
      | Has_no_expressions_of_class templates ->
          not
            (Subject.Statement.On_expressions.exists stm
               ~f:(Act_fir.Expression_class.rec_matches_any ~templates)))

  let check (check : t) ~(stm : Subject.Statement.t) : unit Or_error.t =
    Tx.Or_error.unless_m (is_ok check ~stm) ~f:(fun () ->
        Or_error.error_s
          [%message "Statement failed check" ~check:(check : t)])
end

type t =
  { req_flags: Set.M(Path_flag).t
  ; not_flags: Set.M(Path_flag).t
  ; end_checks: Set.M(End_check).t
  ; threads: Set.M(Int).t option
  ; anchor: Anchor.t option
  ; block: Block.chk option }

let zero : t =
  { req_flags= Set.empty (module Path_flag)
  ; not_flags= Set.empty (module Path_flag)
  ; end_checks= Set.empty (module End_check)
  ; threads= None
  ; anchor= None
  ; block= None }

let ( + ) (l : t) (r : t) : t =
  { req_flags= Set.union l.req_flags r.req_flags
  ; not_flags= Set.union l.not_flags r.not_flags
  ; end_checks= Set.union l.end_checks r.end_checks
  ; threads= Option.merge ~f:Set.inter l.threads r.threads
  ; anchor= Option.merge ~f:Anchor.merge l.anchor r.anchor
  ; block= Option.merge ~f:Block.merge l.block r.block }

let add_if (x : t) ~(when_ : bool) ~(add : t) : t =
  if when_ then x + add else x

let anchor (anc : Anchor.t) : t = {zero with anchor= Some anc}

let ends_in_block (blk : Block.t) : t = {zero with block= Some (Valid blk)}

let require_flags (req_flags : Set.M(Path_flag).t) : t = {zero with req_flags}

let require_flag (req_flag : Path_flag.t) : t =
  require_flags (Set.singleton (module Path_flag) req_flag)

let forbid_flags (not_flags : Set.M(Path_flag).t) : t = {zero with not_flags}

let forbid_flag (not_flag : Path_flag.t) : t =
  forbid_flags (Set.singleton (module Path_flag) not_flag)

let require_end_checks (end_checks : Set.M(End_check).t) : t =
  {zero with end_checks}

let require_end_check (end_check : End_check.t) : t =
  require_end_checks (Set.singleton (module End_check) end_check)

let transaction_safe : t =
  require_end_checks
    (Set.of_list
       (module End_check)
       [ Stm_class (Has_not_any, [Act_fir.Statement_class.atomic ()])
       ; Has_no_expressions_of_class [Atomic None] ])

let live_loop_surround : t =
  (* Don't surround breaks and continues in live code; doing so causes them
     to affect the new surrounding loop, which is a semantic change.

     Note that this does NOT forbid loop-unsafe statements when surrounding;
     this is because some loops are known to execute once only, and so are ok
     to use with such statements. *)
  require_end_check
    (Stm_class
       ( Has_not_any
       , Fir.Statement_class.
           [ Prim (Some (Early_out (Some Break)))
           ; Prim (Some (Early_out (Some Continue))) ] ))

let in_threads_only (threads : Set.M(Int).t) : t =
  {zero with threads= Some threads}

let error_of_flag (flag : Path_flag.t) ~(polarity : string) : unit Or_error.t
    =
  Or_error.errorf "Unmet %s flag condition: %s" polarity
    (Path_flag.to_string flag)

let error_of_flags (flags : Set.M(Path_flag).t) ~(polarity : string) :
    unit Or_error.t =
  flags |> Set.to_list
  |> Tx.Or_error.combine_map_unit ~f:(error_of_flag ~polarity)

let is_thread_ok (filter : t) ~(thread : int) : bool =
  Option.for_all ~f:(fun t -> Set.mem t thread) filter.threads

let check_req (filter : t) ~(flags : Set.M(Path_flag).t) : unit Or_error.t =
  Or_error.Let_syntax.(
    (* This might not be the best place to put this check, but it is a point
       where we have all of the flags that will be enabled on the path. *)
    let%bind _ = Path_flag.check_contradiction_free flags in
    error_of_flags ~polarity:"required" (Set.diff filter.req_flags flags))

let check_not (filter : t) ~(flags : Set.M(Path_flag).t) : unit Or_error.t =
  error_of_flags ~polarity:"forbidden" (Set.inter filter.not_flags flags)

let check_anchor (filter : t) ~(path : Path.Stms.t) ~(block_len : int) :
    unit Or_error.t =
  Tx.Option.With_errors.iter_m filter.anchor
    ~f:(Anchor.check_anchor ~path ~block_len)

let check_block (filter : t) ~(block : Block.t) : unit Or_error.t =
  Tx.Option.With_errors.iter_m filter.block ~f:(Block.check ~block)

let end_checks_for_statement (filter : t) (stm : Subject.Statement.t) :
    unit Or_error.t list =
  filter.end_checks |> Set.to_list |> List.map ~f:(End_check.check ~stm)

let check_final_statement (filter : t) ~(stm : Subject.Statement.t) :
    unit Or_error.t =
  stm |> end_checks_for_statement filter |> Or_error.combine_errors_unit
