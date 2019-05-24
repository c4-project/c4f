(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to permit
   persons to whom the Software is furnished to do so, subject to the
   following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
   NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
   DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
   OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
   USE OR OTHER DEALINGS IN THE SOFTWARE. *)

open Base
open Act_common
module Tx = Travesty_base_exts
open Act_utils

module State_line = struct
  type t = {occurrences: int option; rest: string}
end

module type Basic = Reader_intf.Basic with type state_line := State_line.t

module Automaton = struct
  (** The current automaton state of a Herd reader. *)
  type t =
    | Empty  (** We haven't read anything yet. *)
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
    | State {left} ->
        Validate.fail_s (expected_more_states_message left)
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

  let try_enter_preamble : t -> t Or_error.t = function
    | Empty ->
        Or_error.return Preamble
    | (State _ | Preamble | Summary | Postamble) as st ->
        state_change_error st "entering a preamble"

  (** [try_leave_state a] tries to update [a] after having processed a
      single state line. *)
  let try_leave_state : t -> t Or_error.t = function
    | State {left= 0} ->
        Or_error.error_string "State underflow."
    | State {left= k} ->
        Or_error.return (after_state_line k)
    | (Preamble | Summary | Postamble | Empty) as st ->
        state_change_error st "leaving a state line"

  let try_enter_state_block (num_states : int) : t -> t Or_error.t =
    function
    | Preamble ->
        of_state_count num_states
    | (State _ | Summary | Postamble | Empty) as st ->
        state_change_error st "entering the state block"

  let try_leave_summary : t -> t Or_error.t = function
    | Summary ->
        Or_error.return Postamble
    | (State _ | Preamble | Postamble | Empty) as st ->
        state_change_error st "leaving the summary"
end

module Summary_info = struct
  type t = Defined | Undefined

  let is_defined : t -> bool = function
    | Defined ->
        true
    | Undefined ->
        false
end

module Ctx = struct
  module Body = struct
    type t =
      { a_state: Automaton.t  (** The current automaton state. *)
      ; path: string option  (** The file, if any, we're reading. *)
      ; obs: Act_sim.Output.Observation.t
            (** The observations we've built so far. *) }
    [@@deriving fields, make]

    let init : ?path:string -> unit -> t =
      make ~a_state:Empty ~obs:(Act_sim.Output.Observation.init ())

    let map_fld_m (body : t) ~(field : (t, 'a) Field.t)
        ~(f : 'a -> 'a Or_error.t) : t Or_error.t =
      Or_error.(body |> Field.get field |> f >>| Field.fset field body)

    let map_a_state_m (body : t) :
        f:(Automaton.t -> Automaton.t Or_error.t) -> t Or_error.t =
      map_fld_m body ~field:Fields.a_state

    let map_obs_m (body : t) :
           f:(   Act_sim.Output.Observation.t
              -> Act_sim.Output.Observation.t Or_error.t)
        -> t Or_error.t =
      map_fld_m body ~field:Fields.obs

    let try_enter_preamble : t -> t Or_error.t =
      map_a_state_m ~f:Automaton.try_enter_preamble

    let try_enter_state_block (num_states : int) : t -> t Or_error.t =
      map_a_state_m ~f:(Automaton.try_enter_state_block num_states)

    let try_leave_state (state : Act_sim.State.t) (body : t) : t Or_error.t
        =
      Or_error.(
        body
        |> map_a_state_m ~f:Automaton.try_leave_state
        >>= map_obs_m ~f:(Act_sim.Output.Observation.add ~state))

    let try_set_undefined : t -> t Or_error.t =
      map_obs_m ~f:Act_sim.Output.Observation.set_undefined

    let try_leave_summary (info : Summary_info.t) (body : t) : t Or_error.t
        =
      let is_defined = Summary_info.is_defined info in
      Or_error.(
        body
        |> map_a_state_m ~f:Automaton.try_leave_summary
        >>= Tx.Or_error.map_unless_m is_defined ~f:try_set_undefined)
  end

  include Travesty.State_transform.Make (struct
    type t = Body.t

    module Inner = Or_error
  end)

  let peek_automaton () : Automaton.t t = peek Body.a_state

  let peek_obs () : Act_sim.Output.Observation.t t = peek Body.obs

  (** [fail_if_bad_state ()] produces an error if the reader ended in a
      state other than Postamble. *)
  let fail_if_bad_state () : unit t =
    peek_automaton () >>| Automaton.validate_final_state >>| Validate.result
    >>= Monadic.return

  let try_close () : Act_sim.Output.t t =
    Let_syntax.(
      let%bind _ = fail_if_bad_state () in
      let%map obs = peek_obs () in
      Act_sim.Output.Success obs)

  let run_to_output ?(path : string option) (op : unit t) :
      Act_sim.Output.t Or_error.t =
    run (op >>= try_close) (Body.init ?path ())

  let enter_preamble () : unit t = Monadic.modify Body.try_enter_preamble

  let enter_state_block (num_states : int) : unit t =
    Monadic.modify (Body.try_enter_state_block num_states)

  let leave_state (parsed_state : Act_sim.State.t) : unit t =
    Monadic.modify (Body.try_leave_state parsed_state)

  let leave_summary (info : Summary_info.t) : unit t =
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
            f x line) )

    let iter_m (ic : Stdio.In_channel.t) ~(f : string -> unit M.t) :
        unit M.t =
      fold_m ic ~f:(Fn.const f) ~init:()
  end
end

module Make_main (B : Basic) = struct
  module Preamble = struct
    let process (line : string) : unit Ctx.t =
      let state_o = B.try_parse_state_count line in
      Option.value_map state_o ~f:Ctx.enter_state_block
        ~default:(Ctx.return ())

    let process_from_empty (line : string) : unit Ctx.t =
      Ctx.(enter_preamble () >> process line)
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

    (* The RHS of state lines are always 'binding; binding; binding;', with
       a trailing ;. *)
    let try_parse_state_line_body (line : string) :
        Act_sim.State.t Or_error.t =
      Or_error.(
        line |> String.split ~on:';'
        |> Tx.List.exclude ~f:String.is_empty (* Drop trailing ; *)
        |> List.map ~f:proc_binding |> Result.all >>= Act_sim.State.of_alist)

    let try_parse_state_line (line : string) : Act_sim.State.t Or_error.t =
      Or_error.Let_syntax.(
        let%bind {rest; _} = B.try_split_state_line line in
        try_parse_state_line_body rest)

    let process (line : string) : unit Ctx.t =
      Ctx.(line |> try_parse_state_line |> Monadic.return >>= leave_state)
  end

  module Summary = struct
    (** [summaries] associates each expected summary line in a Herd output
        with some information about it---presently, just whether it
        represents undefined behaviour. *)
    let summaries : (string, Summary_info.t) List.Assoc.t =
      [ ("Ok", Defined)
      ; ("No", Defined)
      ; ("Yes", Defined)
      ; ("Undef", Undefined) ]

    let get_summary_info (line : string) : Summary_info.t Or_error.t =
      line
      |> List.Assoc.find summaries ~equal:String.Caseless.equal
      |> Result.of_option
           ~error:(Error.create_s [%message "unexpected summary" ~got:line])

    let process (line : string) : unit Ctx.t =
      Ctx.Let_syntax.(
        let%bind info = Ctx.Monadic.return (get_summary_info line) in
        Ctx.leave_summary info)
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
          Preamble.process_from_empty
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
  Loadable_intf.Basic with type t = Act_sim.Output.t = struct
  module M = Make_main (B)
  module L = Tx.List.On_monad (Ctx)
  module I = Ic.On_monad (Ctx)

  type nonrec t = Act_sim.Output.t

  let load_from_string (s : string) : Act_sim.Output.t Or_error.t =
    let lines = String.split_lines s in
    Ctx.run_to_output (L.iter_m lines ~f:M.process_line)

  let load_from_ic ?path ic =
    Ctx.run_to_output ?path (I.iter_m ic ~f:M.process_line)
end

module Make (B : Basic) : Act_sim.Reader_intf.S =
  Act_sim.Reader.Make (Loadable.Make (Make_load (B)))
