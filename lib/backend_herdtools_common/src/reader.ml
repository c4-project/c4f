(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Act_common
module Tx = Travesty_base_exts
module Obs = Act_state.Observation
module Tag = Obs.Entry_tag

module Test_type = struct
  type t = Allowed | Required [@@deriving sexp_of]

  let of_string_err (s : string) : t Or_error.t =
    s |> String.strip |> String.lowercase
    |> function
    | "allowed" ->
        Or_error.return Allowed
    | "required" ->
        Or_error.return Required
    | s' ->
        Or_error.error_s [%message "Invalid test type" ~test_type:s']
end

module State_line = struct
  type 'a t =
    { occurrences: int option
    ; tag: Tag.t [@default Tag.Unknown]
    ; rest: 'a [@main] }
  [@@deriving make]

  module T = Travesty.Traversable.Make1 (struct
    type nonrec 'a t = 'a t

    module On_monad (M : Monad.S) = struct
      let map_m (sl : 'a t) ~(f : 'a -> 'b M.t) : 'b t M.t =
        M.Let_syntax.(
          let%map rest' = f sl.rest in
          {occurrences= sl.occurrences; tag= sl.tag; rest= rest'})
    end
  end)

  include (T : module type of T with type 'a t := 'a t)
end

module type Basic =
  Reader_types.Basic
    with type state_line := string State_line.t
     and type test_type := Test_type.t

module Automaton = struct
  (** The current automaton state of a Herd reader. *)
  type t =
    | Empty  (** We haven't read anything yet. *)
    | Pre_test  (** We haven't hit the actual test yet. *)
    | Preamble  (** We're in the pre-state matter. *)
    | State of {left: int}  (** We're in a state block. *)
    | Summary  (** We're reading the summary tag. *)
    | Postamble  (** We're in the post-summary matter. *)
  [@@deriving sexp_of]

  let after_state_line (last_left : int) : t =
    let left = last_left - 1 in
    if left = 0 then Summary else State {left}

  let expected_more_states_message (num_states : int) : Sexp.t =
    [%message
      "Input ended while expecting more states."
        ~num_states:(num_states : int)]

  let negative_state_count_message (num_states : int) : Sexp.t =
    [%message
      "Input declared a negative number of states."
        ~num_states:(num_states : int)]

  let validate_final_state : t Validate.check = function
    | Empty ->
        Validate.fail_s [%message "Input was empty."]
    | State {left; _} ->
        Validate.fail_s (expected_more_states_message left)
    | Pre_test ->
        Validate.fail_s [%message "Input ended before reaching test."]
    | Preamble ->
        Validate.fail_s [%message "Input ended with no state block."]
    | Summary ->
        Validate.fail_s [%message "Input ended while expecting summary."]
    | Postamble ->
        Validate.pass

  let of_state_count (num_states : int) : t Or_error.t =
    match Int.compare num_states 0 with
    | -1 ->
        Or_error.error_s (negative_state_count_message num_states)
    | 0 ->
        Or_error.return Summary
    | _ ->
        Or_error.return (State {left= num_states})

  let state_change_error (st : t) (state : string) : t Or_error.t =
    Or_error.error_s
      [%message
        "In wrong position to change reader state." ~state
          ~at_position:(st : t)]

  let try_enter_pre_test : t -> t Or_error.t = function
    | Empty ->
        Or_error.return Pre_test
    | (State _ | Pre_test | Preamble | Summary | Postamble) as st ->
        state_change_error st "entering a preamble"

  let try_enter_preamble : t -> t Or_error.t = function
    | Pre_test ->
        Or_error.return Preamble
    | (Empty | State _ | Preamble | Summary | Postamble) as st ->
        state_change_error st "entering a preamble"

  (** [try_leave_state a] tries to update [a] after having processed a
      single state line. *)
  let try_leave_state : t -> t Or_error.t = function
    | State {left= 0; _} ->
        Or_error.error_string "State underflow."
    | State {left= k} ->
        Or_error.return (after_state_line k)
    | (Pre_test | Preamble | Summary | Postamble | Empty) as st ->
        state_change_error st "leaving a state line"

  let try_enter_state_block (num_states : int) : t -> t Or_error.t =
    function
    | Preamble ->
        of_state_count num_states
    | (Pre_test | State _ | Summary | Postamble | Empty) as st ->
        state_change_error st "entering the state block"

  let try_leave_summary : t -> t Or_error.t = function
    | Summary ->
        Or_error.return Postamble
    | (Pre_test | State _ | Preamble | Postamble | Empty) as st ->
        state_change_error st "leaving the summary"
end

module Ctx = struct
  module Body = struct
    type t =
      { a_state: Automaton.t  (** The current automaton state. *)
      ; path: string option  (** The file, if any, we're reading. *)
      ; test_type: Test_type.t option  (** The type of test. *)
      ; obs: Act_state.Observation.t
            (** The observations we've built so far. *) }
    [@@deriving fields, make]

    let init : ?path:string -> unit -> t =
      make ~a_state:Empty ~obs:Obs.empty ?test_type:None

    let map_fld_m (body : t) ~(field : (t, 'a) Field.t)
        ~(f : 'a -> 'a Or_error.t) : t Or_error.t =
      Or_error.(body |> Field.get field |> f >>| Field.fset field body)

    let set_test_type (body : t) ~(test_type : Test_type.t) : t =
      {body with test_type= Some test_type}

    let map_a_state_m (body : t) :
        f:(Automaton.t -> Automaton.t Or_error.t) -> t Or_error.t =
      map_fld_m body ~field:Fields.a_state

    let map_obs_m (body : t) : f:(Obs.t -> Obs.t Or_error.t) -> t Or_error.t
        =
      map_fld_m body ~field:Fields.obs

    let try_enter_pre_test : t -> t Or_error.t =
      map_a_state_m ~f:Automaton.try_enter_pre_test

    let try_enter_preamble : t -> t Or_error.t =
      map_a_state_m ~f:Automaton.try_enter_preamble

    let try_enter_state_block (num_states : int) : t -> t Or_error.t =
      map_a_state_m ~f:(Automaton.try_enter_state_block num_states)

    let try_leave_state (sl : Act_state.Entry.t State_line.t) :
        t -> t Or_error.t =
      let {State_line.rest= state; tag; _} = sl in
      Tx.Or_error.(
        map_a_state_m ~f:Automaton.try_leave_state
        >=> map_obs_m ~f:(Act_state.Observation.add ~tag ~entry:state))

    let try_leave_summary (info : Obs.t -> Obs.t Or_error.t) :
        t -> t Or_error.t =
      Tx.Or_error.(
        map_a_state_m ~f:Automaton.try_leave_summary >=> map_obs_m ~f:info)
  end

  include Travesty.State_transform.Make (struct
    type t = Body.t

    module Inner = Or_error
  end)

  let peek_test_type () : Test_type.t option t = peek Body.test_type

  let peek_automaton () : Automaton.t t = peek Body.a_state

  let peek_obs () : Act_state.Observation.t t = peek Body.obs

  (** [fail_if_bad_state ()] produces an error if the reader ended in a
      state other than Postamble. *)
  let fail_if_bad_state () : unit t =
    peek_automaton () >>| Automaton.validate_final_state >>| Validate.result
    >>= Monadic.return

  let try_close () : Act_state.Observation.t t =
    Let_syntax.(
      let%bind _ = fail_if_bad_state () in
      peek_obs ())

  let run_to_output ?(path : string option) (op : unit t) :
      Act_state.Observation.t Or_error.t =
    run (op >>= try_close) (Body.init ?path ())

  let enter_pre_test () : unit t = Monadic.modify Body.try_enter_pre_test

  let enter_preamble () : unit t = Monadic.modify Body.try_enter_preamble

  let set_test_type (test_type : Test_type.t) : unit t =
    modify (Body.set_test_type ~test_type)

  let enter_state_block (num_states : int) : unit t =
    Monadic.modify (Body.try_enter_state_block num_states)

  let leave_state (parsed_state : Act_state.Entry.t State_line.t) : unit t =
    Monadic.modify (Body.try_leave_state parsed_state)

  let leave_summary (info : Obs.t -> Obs.t Or_error.t) : unit t =
    Monadic.modify (Body.try_leave_summary info)
end

(* TODO(@MattWindsor91): generalise and move to Travesty. *)
module Ic = struct
  module On_monad (M : Monad.S) = struct
    let fold_m (ic : Stdio.In_channel.t) ~(f : 'a -> string -> 'a M.t)
        ~(init : 'a) : 'a M.t =
      Stdio.In_channel.fold_lines ic ~init:(M.return init)
        ~f:(fun xm line ->
          M.Let_syntax.(
            let%bind x = xm in
            f x line))

    let iter_m (ic : Stdio.In_channel.t) ~(f : string -> unit M.t) :
        unit M.t =
      fold_m ic ~f:(Fn.const f) ~init:()
  end
end

module Make_main (B : Basic) = struct
  module Pre_test = struct
    let splits_to_test_type : string list -> Test_type.t Or_error.t =
      function
      | _ :: _ :: type_str :: _ ->
          Test_type.of_string_err type_str
      | splits ->
          Or_error.error_s
            [%message
              "Expected three elements in the test header"
                ~got:(splits : string list)]

    let try_parse_test_header (line : string) : Test_type.t Ctx.t =
      line |> String.strip |> String.split ~on:' '
      |> Tx.List.exclude ~f:String.is_empty
      |> splits_to_test_type |> Ctx.Monadic.return

    let process (line : string) : unit Ctx.t =
      if String.is_prefix line ~prefix:"Test" then
        Ctx.(
          line |> try_parse_test_header >>= set_test_type >>= enter_preamble)
      else Ctx.return ()

    let process_from_empty (line : string) : unit Ctx.t =
      Ctx.(enter_pre_test () >> process line)
  end

  module Preamble = struct
    let process (line : string) : unit Ctx.t =
      let state_o = B.try_parse_state_count line in
      Option.value_map state_o ~f:Ctx.enter_state_block
        ~default:(Ctx.return ())
  end

  module State = struct
    let proc_binding (binding : string) : (Litmus_id.t * string) Or_error.t
        =
      match String.split ~on:'=' (String.strip binding) with
      | [l; r] ->
          let open Or_error.Let_syntax in
          let%map l' = Litmus_id.try_parse (String.strip l) in
          (l', String.strip r)
      | _ ->
          Or_error.error_s
            [%message "Expected a binding of the form X=Y" ~got:binding]

    let split_state_bindings (line : string) : string list =
      (* The RHS of state lines are always 'binding; binding; binding;',
         with a trailing ;. *)
      line |> String.split ~on:';' |> Tx.List.exclude ~f:String.is_empty

    (* Drop trailing ; *)

    let try_parse_state_line_body (line : string) :
        Act_state.Entry.t Or_error.t =
      Or_error.(
        line |> split_state_bindings |> List.map ~f:proc_binding
        |> Result.all >>= Act_state.Entry.of_alist)

    let try_parse_state_line (tt : Test_type.t) :
        string -> Act_state.Entry.t State_line.t Or_error.t =
      Tx.Or_error.(
        B.try_split_state_line tt
        >=> State_line.With_errors.map_m ~f:try_parse_state_line_body)

    let process (line : string) : unit Ctx.t =
      Ctx.(
        Let_syntax.(
          let%bind tt_opt = peek_test_type () in
          let%bind tt =
            Monadic.return
              (Result.of_option tt_opt
                 ~error:
                   (Error.of_string "Internal error: Missing test type"))
          in
          let%bind state = Monadic.return (try_parse_state_line tt line) in
          leave_state state))
  end

  module Summary = struct
    (** [summaries] associates each expected summary line in a Herd output
        with the observation flags it represents. *)
    let summaries : (string, Obs.t -> Obs.t Or_error.t) List.Assoc.t =
      [ ("Ok", Obs.set_sat)
      ; ("No", Obs.set_unsat)
      ; ("Yes", Obs.set_sat) (* seen in practice? *)
      ; ("Undef", Obs.set_undefined) ]

    let get_summary_info (line : string) (obs : Obs.t) : Obs.t Or_error.t =
      let f =
        line
        |> List.Assoc.find summaries ~equal:String.Caseless.equal
        |> Result.of_option
             ~error:
               (Error.create_s [%message "unexpected summary" ~got:line])
      in
      Or_error.bind f ~f:(fun f' -> f' obs)

    let process (line : string) : unit Ctx.t =
      Ctx.leave_summary (get_summary_info line)
  end

  module Postamble = struct
    let process (_line : string) : unit Ctx.t =
      (* TODO(@MattWindsor91): do something with this? *)
      Ctx.return ()
  end

  let get_processor () : (string -> unit Ctx.t) Ctx.t =
    Ctx.Let_syntax.(
      match%map Ctx.peek_automaton () with
      | Empty ->
          Pre_test.process_from_empty
      | Pre_test ->
          Pre_test.process
      | Preamble ->
          Preamble.process
      | State _ ->
          State.process
      | Summary ->
          Summary.process
      | Postamble ->
          Postamble.process)

  let process_line (line : string) : unit Ctx.t =
    (* Make parsing the line easier by compressing whitespace. *)
    let line' = String.strip (String_extended.squeeze line) in
    Ctx.(get_processor () >>= fun f -> f line')
end

module Make_load (B : Basic) :
  Plumbing.Loadable_types.Basic with type t = Act_state.Observation.t =
struct
  module M = Make_main (B)
  module L = Tx.List.On_monad (Ctx)
  module I = Ic.On_monad (Ctx)

  type nonrec t = Act_state.Observation.t

  let load_from_string (s : string) : Act_state.Observation.t Or_error.t =
    let lines = String.split_lines s in
    Ctx.run_to_output (L.iter_m lines ~f:M.process_line)

  let load_from_ic ?(path : string option) (ic : Stdio.In_channel.t) :
      Obs.t Or_error.t =
    Ctx.run_to_output ?path (I.iter_m ic ~f:M.process_line)
end

module Make (B : Basic) : Act_backend.Reader_types.S =
  Plumbing.Loadable.Make (Make_load (B))
