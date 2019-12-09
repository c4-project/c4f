(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

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

  let check (check : t) ~(stm : Subject.Statement.t) : bool =
    match check with
    | Is_if_statement ->
        Act_c_mini.Statement.is_if_statement stm
end

type t =
  { obs_flags: Set.M(Flag).t
  ; req_flags: Set.M(Flag).t
  ; end_checks: Set.M(End_check).t }

let empty : t =
  { obs_flags= Set.empty (module Flag)
  ; req_flags= Set.empty (module Flag)
  ; end_checks= Set.empty (module End_check) }

let require_flag (existing : t) ~(flag : Flag.t) : t =
  {existing with req_flags= Set.add existing.req_flags flag}

let require_end_check (existing : t) ~(check : End_check.t) : t =
  {existing with end_checks= Set.add existing.end_checks check}

let in_dead_code_only : t -> t = require_flag ~flag:In_dead_code

let final_if_statements_only : t -> t =
  require_end_check ~check:Is_if_statement

let update_with_block_metadata (existing : t) (m : Metadata.t) : t =
  let new_flags =
    Flag.metadata_predicates
    |> List.filter_map ~f:(fun (flag, predicate) ->
           Option.some_if (predicate m) flag)
    |> Set.of_list (module Flag)
  in
  {existing with obs_flags= Set.union existing.obs_flags new_flags}

let is_ok (filter : t) : bool =
  (* TODO(@MattWindsor91): give a proper or-error here *)
  let unmet_flags = Set.diff filter.req_flags filter.obs_flags in
  Set.is_empty unmet_flags

let is_final_statement_ok (filter : t) ~(stm : Subject.Statement.t) : bool =
  is_ok filter
  && Set.for_all filter.end_checks ~f:(fun x -> End_check.check x ~stm)
