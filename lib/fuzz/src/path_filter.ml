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

let zero : t =
  { req_flags= Set.empty (module Path_flag)
  ; not_flags= Set.empty (module Path_flag)
  ; end_checks= Set.empty (module End_check)
  ; threads= None }

let ( + ) (l : t) (r : t) : t =
  { req_flags= Set.union l.req_flags r.req_flags
  ; not_flags= Set.union l.not_flags r.not_flags
  ; end_checks= Set.union l.end_checks r.end_checks
  ; threads= Option.merge ~f:Set.inter l.threads r.threads }

let add_if (x : t) ~(when_ : bool) ~(add : t) : t =
  if when_ then x + add else x

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
  error_of_flags ~polarity:"required" (Set.diff filter.req_flags flags)

let check_not (filter : t) ~(flags : Set.M(Path_flag).t) : unit Or_error.t =
  error_of_flags ~polarity:"forbidden" (Set.inter filter.not_flags flags)

let end_checks_for_statement (filter : t) (stm : Subject.Statement.t) :
    unit Or_error.t list =
  filter.end_checks |> Set.to_list |> List.map ~f:(End_check.check ~stm)

let check_final_statement (filter : t) ~(stm : Subject.Statement.t) :
    unit Or_error.t =
  stm |> end_checks_for_statement filter |> Or_error.combine_errors_unit
