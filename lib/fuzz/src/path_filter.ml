(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

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

let is_flag_constructible (flag : Path_flag.t) ~(subject : Subject.Test.t) :
    bool =
  (* TODO(@MattWindsor91): unsound: see GitHub issue #187 *)
  match flag with
  | In_loop ->
      Subject.Test.has_statements subject
        ~matching:[Act_fir.Statement_class.while_loop ()]
  | In_dead_code ->
      Subject.Test.exists_top_statement subject
        ~f:
          (Act_fir.Statement.has_blocks_with_metadata
             ~predicate:Metadata.is_dead_code)
  | In_atomic ->
      Subject.Test.has_statements subject
        ~matching:[Act_fir.Statement_class.lock_block ~specifically:Atomic ()]

(** Checks that can only be carried out at the end of a statement path. *)
module End_check = struct
  module M = struct
    type t =
      | Is_of_class of Act_fir.Statement_class.t list
      | Is_not_of_class of Act_fir.Statement_class.t list
      | Has_no_expressions_of_class of Act_fir.Expression_class.t list
    [@@deriving compare, equal, sexp]
  end

  include M
  include Comparable.Make (M)

  let is_ok (check : t) ~(stm : Subject.Statement.t) : bool =
    Act_fir.Statement_class.(
      match check with
      | Is_of_class templates ->
          matches_any stm ~templates
      | Is_not_of_class templates ->
          not (matches_any stm ~templates)
      | Has_no_expressions_of_class templates ->
          not
            (Subject.Statement.On_expressions.exists stm
               ~f:(Act_fir.Expression_class.rec_matches_any ~templates)))

  let is_constructible (flag : t) ~(subject : Subject.Test.t) : bool =
    (* TODO(@MattWindsor91): unsound: see GitHub issue #187 *)
    match flag with
    | Is_of_class matching ->
        Subject.Test.has_statements subject ~matching
    | Is_not_of_class one_of ->
        Subject.Test.has_statements_not_matching subject ~one_of
    | Has_no_expressions_of_class templates ->
        Subject.Test.exists_top_statement subject
          ~f:
            (Subject.Statement.On_expressions.exists
               ~f:(Act_fir.Expression_class.rec_unmatches_any ~templates))

  let check (check : t) ~(stm : Subject.Statement.t) : unit Or_error.t =
    Tx.Or_error.unless_m (is_ok check ~stm) ~f:(fun () ->
        Or_error.error_s
          [%message "Statement failed check" ~check:(check : t)] )
end

type t =
  { obs_flags: Set.M(Path_flag).t
  ; req_flags: Set.M(Path_flag).t
  ; not_flags: Set.M(Path_flag).t
  ; end_checks: Set.M(End_check).t
  ; threads: Set.M(Int).t option }

let empty : t =
  { obs_flags= Set.empty (module Path_flag)
  ; req_flags= Set.empty (module Path_flag)
  ; not_flags= Set.empty (module Path_flag)
  ; end_checks= Set.empty (module End_check)
  ; threads= None }

module Obs_flags = struct
  let observe_flags (existing : t) ~(flags : Set.M(Path_flag).t) : t =
    {existing with obs_flags= Set.union existing.obs_flags flags}

  let update_with_flow ?(flow : Act_fir.Statement_class.Flow.t option)
      (x : t) : t =
    let flags =
      Option.value_map flow ~f:Path_flag.flags_of_flow
        ~default:(Set.empty (module Path_flag))
    in
    observe_flags x ~flags

  let update_with_block_metadata (existing : t) (m : Metadata.t) : t =
    observe_flags existing ~flags:(Path_flag.flags_of_metadata m)
end

include Obs_flags

module Req_flags = struct
  let require_flag (existing : t) ~(flag : Path_flag.t) : t =
    {existing with req_flags= Set.add existing.req_flags flag}

  let in_dead_code_only : t -> t = require_flag ~flag:In_dead_code

  let in_loop_only : t -> t = require_flag ~flag:In_loop
end

include Req_flags

module Not_flags = struct
  let forbid_flag (existing : t) ~(flag : Path_flag.t) : t =
    {existing with not_flags= Set.add existing.not_flags flag}

  let not_in_atomic_block : t -> t = forbid_flag ~flag:In_atomic
end

include Not_flags

let require_end_check (existing : t) ~(check : End_check.t) : t =
  {existing with end_checks= Set.add existing.end_checks check}

let transaction_safe (filter : t) : t =
  (* TODO(@MattWindsor91): add things to this as we go along. *)
  require_end_check
    ~check:(Is_not_of_class [Act_fir.Statement_class.atomic ()])
  @@ require_end_check
       ~check:(Has_no_expressions_of_class [Atomic None])
       filter

let in_threads_only (filter : t) ~(threads : Set.M(Int).t) : t =
  let threads' =
    Option.value_map filter.threads ~f:(Set.inter threads) ~default:threads
  in
  {filter with threads= Some threads'}

let unmet_req_flags (filter : t) : Set.M(Path_flag).t =
  Set.diff filter.req_flags filter.obs_flags

let unmet_not_flags (filter : t) : Set.M(Path_flag).t =
  Set.inter filter.not_flags filter.obs_flags

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

let check_req : t -> unit Or_error.t =
  Fn.compose (error_of_flags ~polarity:"required") unmet_req_flags

let check_not : t -> unit Or_error.t =
  Fn.compose (error_of_flags ~polarity:"forbidden") unmet_not_flags

let check (filter : t) : unit Or_error.t =
  Or_error.Let_syntax.(
    let%bind () = check_req filter in
    check_not filter)

let end_checks_for_statement (filter : t) (stm : Subject.Statement.t) :
    unit Or_error.t list =
  filter.end_checks |> Set.to_list |> List.map ~f:(End_check.check ~stm)

let check_final_statement (filter : t) ~(stm : Subject.Statement.t) :
    unit Or_error.t =
  let end_checks = end_checks_for_statement filter stm in
  Or_error.combine_errors_unit (check filter :: end_checks)

let are_final_statements_ok (filter : t)
    ~(all_stms : Subject.Statement.t list) ~(pos : int) ~(len : int) : bool =
  Set.is_empty filter.end_checks
  ||
  let stms' = Sequence.(sub (of_list all_stms) ~pos ~len) in
  Sequence.for_all stms' ~f:(fun stm ->
      Set.for_all filter.end_checks ~f:(End_check.is_ok ~stm) )

module Construct_checks = struct
  let is_constructible_req (filter : t) ~(subject : Subject.Test.t) : bool =
    Set.for_all filter.req_flags ~f:(is_flag_constructible ~subject)

  let is_constructible_end (filter : t) ~(subject : Subject.Test.t) : bool =
    Set.for_all filter.end_checks ~f:(End_check.is_constructible ~subject)

  let threads_to_remove (threads_to_keep : Set.M(Int).t)
      ~(subject : Subject.Test.t) : Set.M(Int).t =
    let num_all_threads =
      List.length (Act_litmus.Test.Raw.threads subject)
    in
    let all_threads =
      Set.of_increasing_iterator_unchecked
        (module Int)
        ~len:num_all_threads ~f:Fn.id
    in
    Set.diff all_threads threads_to_keep

  let filter_to_threads (threads : Set.M(Int).t) ~(subject : Subject.Test.t)
      : Subject.Test.t =
    (* Decreasing order to avoid downwards thread ID changes rippling through
       as we try to remove threads. *)
    (* TODO(@MattWindsor91): this is a little heavyweight!! *)
    threads
    |> threads_to_remove ~subject
    |> Set.to_sequence ~order:`Decreasing
    |> Sequence.fold ~init:subject ~f:(fun subject' index ->
           subject'
           |> Act_litmus.Test.Raw.remove_thread ~index
           |> Result.ok
           |> Option.value ~default:subject' )

  let is_constructible (filter : t) ~(subject : Subject.Test.t) : bool =
    let subject =
      Option.value_map filter.threads
        ~f:(filter_to_threads ~subject)
        ~default:subject
    in
    is_constructible_req filter ~subject
    && is_constructible_end filter ~subject
end

include Construct_checks
