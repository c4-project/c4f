(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module Tx = Travesty_base_exts
end

(* This module used to be highly functionalised (with each check being a
   predicate function collected in a list); we replaced this with the present
   flag-based approach to give better error messages when paths fail
   validation. *)

(** Flags that are raised as 'required' before considering a path, then
    lowered as the path progresses through segments of program that have the
    appropriate metadata or shape. *)
module Flag = struct
  module M = struct
    type t = In_loop | In_dead_code [@@deriving enum]

    let table : (t, string) List.Assoc.t =
      [(In_loop, "in loop"); (In_dead_code, "in dead code")]
  end

  include M
  include Act_utils.Enum.Extend_table (M)

  (** Maps a subset of the flags to predicates that toggle whether a piece of
      metadata sets the flag or not. *)
  let metadata_predicates : (t, Metadata.t -> bool) List.Assoc.t =
    [(In_dead_code, Metadata.is_dead_code)]
end

(** Checks that can only be carried out at the end of a statement path. *)
module End_check = struct
  module M = struct
    type t = Is_if_statement [@@deriving enum]

    let table : (t, string) List.Assoc.t =
      [(Is_if_statement, "if statements only")]
  end

  include M
  include Act_utils.Enum.Extend_table (M)

  let is_ok (check : t) ~(stm : Subject.Statement.t) : bool =
    match check with
    | Is_if_statement ->
        Act_c_mini.Statement.is_if_statement stm

  let check (check : t) ~(stm : Subject.Statement.t) : unit Or_error.t =
    Tx.Or_error.unless_m (is_ok check ~stm) ~f:(fun () ->
        Or_error.errorf "Statement failed check: %s" (to_string check))
end

type t =
  { obs_flags: Set.M(Flag).t
  ; req_flags: Set.M(Flag).t
  ; end_checks: Set.M(End_check).t }

let empty : t =
  { obs_flags= Set.empty (module Flag)
  ; req_flags= Set.empty (module Flag)
  ; end_checks= Set.empty (module End_check) }

module Obs_flags = struct
  let observe_flag (existing : t) ~(flag : Flag.t) : t =
    {existing with obs_flags= Set.add existing.obs_flags flag}

  let observe_flags (existing : t) ~(flags : Set.M(Flag).t) : t =
    {existing with obs_flags= Set.union existing.obs_flags flags}

  let update_with_loop : t -> t = observe_flag ~flag:In_loop

  (* Moving this into the Flag module'd require a lot of module gymnastics to
     get the set module correct, so we don't. *)
  let flags_of_metadata (m : Metadata.t) : Set.M(Flag).t =
    Flag.metadata_predicates
    |> List.filter_map ~f:(fun (flag, predicate) ->
           Option.some_if (predicate m) flag)
    |> Set.of_list (module Flag)

  let update_with_block_metadata (existing : t) (m : Metadata.t) : t =
    observe_flags existing ~flags:(flags_of_metadata m)
end

include Obs_flags

module Req_flags = struct
  let require_flag (existing : t) ~(flag : Flag.t) : t =
    {existing with req_flags= Set.add existing.req_flags flag}

  let in_dead_code_only : t -> t = require_flag ~flag:In_dead_code

  let in_loop_only : t -> t = require_flag ~flag:In_loop
end

include Req_flags

module End_checks = struct
  let require_end_check (existing : t) ~(check : End_check.t) : t =
    {existing with end_checks= Set.add existing.end_checks check}

  let final_if_statements_only : t -> t =
    require_end_check ~check:Is_if_statement
end

include End_checks

let unmet_flags (filter : t) : Set.M(Flag).t =
  Set.diff filter.req_flags filter.obs_flags

let error_of_flag (flag : Flag.t) : unit Or_error.t =
  Or_error.errorf "Unmet flag condition: %s" (Flag.to_string flag)

let check (filter : t) : unit Or_error.t =
  filter |> unmet_flags |> Set.to_list
  |> Tx.Or_error.combine_map_unit ~f:error_of_flag

let check_final_statement (filter : t) ~(stm : Subject.Statement.t) :
    unit Or_error.t =
  let end_checks =
    filter.end_checks |> Set.to_list |> List.map ~f:(End_check.check ~stm)
  in
  Or_error.combine_errors_unit (check filter :: end_checks)
