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
open Utils

module State = struct
  (** The current state of a Herd reader. *)
  type t =
    | Empty  (** We haven't read anything yet. *)
    | Preamble  (** We're in the pre-state matter. *)
    | State of int
        (** We're in a state block with the given remaining length. *)
    | Summary  (** We're reading the summary tag. *)
    | Postamble  (** We're in the post-summary matter. *)
    | Error of Error.t  (** We've hit an error. *)

  let after_state_line (states_left : int) : t =
    if states_left = 1 then Summary else State (states_left - 1)

  let validate_final_state : t Validate.check = function
    | Empty ->
        Validate.fail_s [%message "Herd file was empty."]
    | Error e ->
        Validate.of_error (Fn.const (Result.Error e)) ()
    | State k ->
        Validate.fail_s
          [%message
            "Herd file ended while expecting more states."
              ~num_states:(k : int)]
    | Preamble ->
        Validate.fail_s [%message "Herd file ended with no state block."]
    | Summary ->
        Validate.fail_s
          [%message "Herd file ended while expecting summary."]
    | Postamble ->
        Validate.pass
end

type t =
  { path: string option  (** The file, if any, we're reading *)
  ; output: Sim_output.t  (** The Herd run so far *)
  ; state: State.t  (** Which state are we currently in? *) }

let init ?(path : string option) () : t =
  {path; output= Sim_output.init (); state= Empty}

(** [fail_if_bad_state] produces an error if the reader ended in a state
    other than Postamble. *)
let fail_if_bad_state ({state; _} : t) : unit Or_error.t =
  Validate.result (State.validate_final_state state)

let get_output (r : t) : Sim_output.t Or_error.t =
  let open Or_error.Let_syntax in
  Or_error.tag_arg
    (let%bind () = fail_if_bad_state r in
     return r.output)
    "While reading Herd input from"
    (Option.value ~default:"(stdin)" r.path)
    [%sexp_of: string]

let process_preamble herd line =
  (* Try to work out whether we're getting a 'States K' block. *)
  let proc_state (k : int) : State.t =
    match Int.compare k 0 with
    | -1 ->
        Error (Error.create_s [%message "negative state" ~got:(k : int)])
    | 0 ->
        Summary
    | _ ->
        State k
  in
  let state_o =
    Option.try_with (fun () -> Caml.Scanf.sscanf line "States %d" proc_state)
  in
  let state' = Option.value ~default:Preamble state_o in
  (state', herd)

let proc_binding (binding : string) : (Litmus.Id.t * string) Or_error.t =
  match String.split ~on:'=' (String.strip binding) with
  | [l; r] ->
      let open Or_error.Let_syntax in
      let%map l' = Litmus.Id.try_parse (String.strip l) in
      (l', String.strip r)
  | _ ->
      Or_error.error_s
        [%message "Expected a binding of the form X=Y" ~got:binding]

(* State lines are always 'binding; binding; binding;', with a trailing ;. *)
let try_parse_state_line (line : string) : Sim_output.State.t Or_error.t =
  Or_error.(
    line |> String.split ~on:';'
    |> Travesty.T_list.exclude ~f:String.is_empty (* Drop trailing ; *)
    |> List.map ~f:proc_binding |> Result.all >>= Sim_output.State.of_alist)

let try_process_state_line (states_left : int) (line : string)
    (output : Sim_output.t) : (State.t * Sim_output.t) Or_error.t =
  let open Or_error.Let_syntax in
  let%bind state = try_parse_state_line line in
  let%map output' = Sim_output.add output ~state in
  (State.after_state_line states_left, output')

let wrap_error ~(f : Sim_output.t -> (State.t * Sim_output.t) Or_error.t)
    (out : Sim_output.t) : State.t * Sim_output.t =
  match f out with
  | Ok (state, out') ->
      (state, out')
  | Error e ->
      (Error e, out)

let process_state_line (states_left : int) (line : string) :
    Sim_output.t -> State.t * Sim_output.t =
  wrap_error ~f:(try_process_state_line states_left line)

(** [summaries] associates each expected summary line in a Herd output with
    some information about it---presently, just whether it represents
    undefined behaviour. *)
let summaries : (string, bool) List.Assoc.t =
  [("Ok", false); ("No", false); ("Yes", false); ("Undef", true)]

let try_process_summary (line : string) (output : Sim_output.t) :
    (State.t * Sim_output.t) Or_error.t =
  let open Or_error.Let_syntax in
  let summary = String.strip line in
  let is_undefined_opt =
    List.Assoc.find summaries summary ~equal:String.Caseless.equal
  in
  let%bind is_undefined =
    Result.of_option is_undefined_opt
      ~error:(Error.create_s [%message "unexpected summary" ~got:summary])
  in
  let%map output' =
    (if is_undefined then Sim_output.set_undefined else Or_error.return)
      output
  in
  (State.Postamble, output')

let process_summary (line : string) : Sim_output.t -> State.t * Sim_output.t
    =
  wrap_error ~f:(try_process_summary line)

let process_postamble output _line =
  (* TODO(@MattWindsor91): do something with this? *)
  (State.Postamble, output)

let process_line (rd : t) (line : string) : t =
  (* A Herd file looks like this:

     Test NAME Required <- Empty States 3 <- Preamble ZUt0r0=0; ZUx=1;
     ZUy=1; <- State 3 ZUt0r0=0; ZUx=1; ZUy=2; <- State 2 ZUt0r0=1; ZUx=1;
     ZUy=1; <- State 1 Ok <- Summary Witnesses <- Postamble Positive: 3
     Negative: 0 <- Postamble Condition forall (true) <- Postamble
     Observation NAME Always 3 0 <- Postamble Time NAME 0.00 <- Postamble
     Hash=HASH <- Postamble

     At time of writing, we're only interested in the state set. *)

  (* Make parsing the line easier by compressing whitespace. *)
  let line = String.strip (String_extended.squeeze line) in
  let state', output' =
    match rd.state with
    | Error _ ->
        (rd.state, rd.output)
    | Empty | Preamble ->
        process_preamble rd.output line
    | State 0 ->
        failwith "state underflow"
    | State n ->
        process_state_line n line rd.output
    | Summary ->
        process_summary line rd.output
    | Postamble ->
        process_postamble rd.output line
  in
  {rd with state= state'; output= output'}

module Load : Loadable.Basic with type t = Sim_output.t = struct
  type nonrec t = Sim_output.t

  let load_from_string s =
    s |> String.split_lines
    |> List.fold ~init:(init ()) ~f:process_line
    |> get_output

  let load_from_ic ?path ic =
    ic
    |> Stdio.In_channel.fold_lines ~init:(init ?path ()) ~f:process_line
    |> get_output
end

include Loadable.Make (Load)

let%expect_test "load_from_string on empty string fails" =
  Or_error.iter_error
    ~f:(Fmt.pr "@[<v>%a@]@." Error.pp)
    (load_from_string "") ;
  [%expect
    {|
    ("While reading Herd input from" "(stdin)"
     ("validation errors" (("" "Herd file was empty.")))) |}]
