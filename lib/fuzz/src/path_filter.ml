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
  ; threads: Set.M(Int).t option }

let empty : t =
  { req_flags= Set.empty (module Path_flag)
  ; not_flags= Set.empty (module Path_flag)
  ; end_checks= Set.empty (module End_check)
  ; threads= None }

module Req_flags = struct
  let require_flag (existing : t) ~(flag : Path_flag.t) : t =
    {existing with req_flags= Set.add existing.req_flags flag}

  let req (existing : t) ~(flags : Set.M(Path_flag).t) : t =
    {existing with req_flags= Set.union existing.req_flags flags}

  let in_dead_code_only : t -> t = require_flag ~flag:In_dead_code

  let in_loop_only : t -> t = require_flag ~flag:In_loop
end

include Req_flags

module Not_flags = struct
  let forbid_flag (existing : t) ~(flag : Path_flag.t) : t =
    {existing with not_flags= Set.add existing.not_flags flag}

  let not_in_atomic_block : t -> t = forbid_flag ~flag:In_atomic

  let not_in_execute_multi : t -> t = forbid_flag ~flag:In_execute_multi
end

include Not_flags

let require_end_check (existing : t) ~(check : End_check.t) : t =
  {existing with end_checks= Set.add existing.end_checks check}

let transaction_safe (filter : t) : t =
  (* TODO(@MattWindsor91): add things to this as we go along. *)
  require_end_check
    ~check:(Stm_class (Has_not_any, [Act_fir.Statement_class.atomic ()]))
  @@ require_end_check
       ~check:(Has_no_expressions_of_class [Atomic None])
       filter

let in_threads_only (filter : t) ~(threads : Set.M(Int).t) : t =
  let threads' =
    Option.value_map filter.threads ~f:(Set.inter threads) ~default:threads
  in
  {filter with threads= Some threads'}

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
  error_of_flags ~polarity:"required" (Set.diff filter.req_flags flags)

let check_not (filter : t) ~(flags : Set.M(Path_flag).t) : unit Or_error.t =
  error_of_flags ~polarity:"forbidden" (Set.inter filter.not_flags flags)

let end_checks_for_statement (filter : t) (stm : Subject.Statement.t) :
    unit Or_error.t list =
  filter.end_checks |> Set.to_list |> List.map ~f:(End_check.check ~stm)

let check_final_statement (filter : t) ~(stm : Subject.Statement.t) :
    unit Or_error.t =
  stm |> end_checks_for_statement filter |> Or_error.combine_errors_unit
