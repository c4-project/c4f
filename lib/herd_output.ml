(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation
   files (the "Software"), to deal in the Software without
   restriction, including without limitation the rights to use, copy,
   modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

open Core_kernel
open Utils

module Id = Litmus.Id

module State = struct
  module M = struct
    type t = string Id.Map.t [@@deriving sexp, compare]
  end

  include M

  let of_alist = Id.Map.of_alist_or_error

  let bound = Id.Map.keys

  let map
      ~(keyf : Id.t -> Id.t option Or_error.t)
      ~(valf : string -> string Or_error.t)
      state =
    let open Or_error.Let_syntax in
    let f (k, v) =
      let%bind ko = keyf k in
      let%map  v' = valf v in
      let open Option.Let_syntax in
      let%map  k' = ko in
      (k', v')
    in
    let      alist  = Id.Map.to_alist state in
    (* First, fail the map if there were any errors... *)
    let%bind mapped = Or_error.combine_errors (List.map ~f alist) in
    (* Next, remove any items with no key mapping... *)
    let      alist' = List.filter_opt mapped in
    Id.Map.of_alist_or_error alist'
  ;;

  module Set = My_set.Extend (Set.Make (M))
end

type t =
  { states   : State.t list
  ; is_undef : bool
  } [@@deriving fields]
;;

(** [init ()] generates an initial [t]. *)
let init () =
  { states   = []
  ; is_undef = false
  }

type single_outcome =
  [ `Unknown
  | `Undef
  ]
[@@deriving sexp]
;;

type outcome =
  [ single_outcome
  | `Order of State.Set.Partial_order.t
  | `OracleUndef
  ]
[@@deriving sexp]
;;

let single_outcome_of (herd : t) : single_outcome =
  if herd.is_undef then `Undef else `Unknown
;;

let compare_states ~initials ~finals
    ~(locmap : Id.t -> Id.t option Or_error.t)
    ~(valmap : string -> string Or_error.t)
  : outcome Or_error.t =
  let open Or_error.Let_syntax in
  let f = State.map ~keyf:locmap ~valf:valmap in
  let%map finals' = Or_error.combine_errors (List.map ~f finals) in
  let result =
    State.Set.(Travesty.T_fn.on of_list partial_compare initials finals')
  in
  `Order result
;;

let outcome_of ~initial ~final ~locmap ~valmap : outcome Or_error.t =
  if initial.is_undef
  then Or_error.return `OracleUndef
  else if final.is_undef
  then Or_error.return `Undef
  else
    compare_states
      ~initials:initial.states ~finals:final.states ~locmap ~valmap
;;

(** [rstate] is the current state of a Herd reader. *)
type rstate =
  | Empty            (** We haven't read anything yet. *)
  | Preamble         (** We're in the pre-state matter. *)
  | State of int     (** We're in a state block with the given
                         remaining length. *)
  | Summary          (** We're reading the summary tag. *)
  | Postamble        (** We're in the post-summary matter. *)
  | Error of Error.t (** We've hit an error. *)
;;

(** [reader] is the state structure used when reading in a Herd run. *)
type reader =
  { path  : string option  (** The file, if any, we're reading *)
  ; herd  : t              (** The Herd run so far *)
  ; state : rstate         (** Which state are we currently in? *)
  }

(** [init_reader path] generates an initial [reader], with optional
    path [path]. *)
let init_reader (path : string option) : reader =
  { path
  ; herd  = init ()
  ; state = Empty
  }

(** [fail_if_bad_state] produces an error if the reader ended in a
    state other than Postamble. *)
let fail_if_bad_state ({ state;  _} : reader) : unit Or_error.t =
  match state with
  | Empty ->
    Or_error.error_s
      [%message "Herd file was empty."]
  | Error e ->
    Result.Error e
  | State k ->
    Or_error.error_s
      [%message "Herd file ended while expecting more states."
          ~num_states:(k : int)]
  | Preamble ->
    Or_error.error_s
      [%message "Herd file ended with no state block."]
  | Summary ->
    Or_error.error_s
      [%message "Herd file ended while expecting summary."]
  | Postamble -> Result.ok_unit
;;

(** [validate r] runs some checks on the result of processing
   a Herd file using reader [r]. *)
let validate (r : reader) : t Or_error.t =
  let open Or_error.Let_syntax in
  Or_error.tag_arg
    begin
      let%bind () = fail_if_bad_state r in
      return r.herd
    end
    "While reading Herd input from"
    (Option.value ~default:"(stdin)" r.path)
    [%sexp_of: string]
;;

let process_preamble herd line =
  (* Try to work out whether we're getting a 'States K' block. *)
  let proc_state k =
    match Int.compare k 0 with
    | -1 -> Error (Error.create_s [%message "negative state" ~got:(k: int)])
    |  0 -> Summary
    |  _ -> State k
  in
  let state_o =
    Option.try_with
      (fun () -> Scanf.sscanf line "States %d" proc_state)
  in
  let state' = Option.value ~default:Preamble state_o in
  (state', herd)
;;

let proc_binding (binding : string)
  : (Id.t * string) Or_error.t =
  match String.split ~on:'=' (String.strip binding) with
  | [l; r] ->
    let open Or_error.Let_syntax in
    let%map l' = Id.try_parse (String.strip l) in
    (l', String.strip r)
  | _ ->
    Or_error.error_s
      [%message
        "Expected a binding of the form X=Y"
          ~got:binding
      ]
;;

let process_state n herd line =


  (* State lines are always 'binding; binding; binding;', with
     a trailing ;. *)
  let state_line =
    Or_error.(
      line
      |> String.split ~on:';'
      |> Travesty.T_list.exclude ~f:String.is_empty (* Drop trailing ; *)
      |> List.map ~f:proc_binding
      |> Result.all
      >>= State.of_alist
    )
  in

  match state_line with
  | Ok sl ->
    ( (if n = 1 then Summary else State (n - 1))
    , { herd with states = sl::herd.states }
    )
  | Error e -> ( Error e, herd )
;;

let mk_summary_fun is_undef h =
  (Postamble, { h with is_undef = is_undef })
;;

(** [summaries] associates each expected summary line in a Herd
    output with a function generating the next state and
    Herd record. *)
let summaries =
  [ "Ok"   , mk_summary_fun false
  ; "No"   , mk_summary_fun false
  ; "Yes"  , mk_summary_fun false
  ; "Undef", mk_summary_fun true
  ]
;;

let process_summary herd line =
  let summary = String.strip line in
  let summary_op =
    List.Assoc.find
      ~equal:String.Caseless.equal
      summaries
      summary
  in
  (Option.value
     ~default:(
       Tuple2.create
         (Error (Error.create_s
                   [%message "unexpected summary" ~got:summary])))
     summary_op)
    herd
;;

let process_postamble herd _line =
  (* TODO(@MattWindsor91): do something with this? *)
  (Postamble, herd)

let process_line (rd : reader) (line : string) : reader =
  (* A Herd file looks like this:

     Test NAME Required           <- Empty
     States 3                     <- Preamble
     ZUt0r0=0; ZUx=1; ZUy=1;      <- State 3
     ZUt0r0=0; ZUx=1; ZUy=2;      <- State 2
     ZUt0r0=1; ZUx=1; ZUy=1;      <- State 1
     Ok                           <- Summary
     Witnesses                    <- Postamble
     Positive: 3 Negative: 0      <- Postamble
     Condition forall (true)      <- Postamble
     Observation NAME Always 3 0  <- Postamble
     Time NAME 0.00               <- Postamble
     Hash=HASH                    <- Postamble

     At time of writing, we're only interested in the state set.
  *)

  (* Make parsing the line easier by compressing whitespace. *)
  let line =
    String.strip (String_extended.squeeze line)
  in

  let state', herd' =
    match rd.state with
    | Error _   -> (rd.state, rd.herd)
    | Empty
    | Preamble  -> process_preamble rd.herd line
    | State 0   -> failwith "state underflow"
    | State n   -> process_state n rd.herd line
    | Summary   -> process_summary rd.herd line
    | Postamble -> process_postamble rd.herd line
  in { rd with state = state'; herd = herd' }
;;

module Load : Loadable.Basic with type t = t = struct
  type nonrec t = t

  let load_from_string s =
    s
    |> String.split_lines
    |> List.fold ~init:(init_reader None) ~f:process_line
    |> validate
  ;;

  let load_from_ic ?path ic =
    ic
    |> In_channel.fold_lines ~init:(init_reader path) ~f:process_line
    |> validate
  ;;
end

include Loadable.Make (Load);;

let%expect_test "load_from_string on empty string fails" =
  Or_error.iter_error
    ~f:(Format.printf "@[<v>%a@]@." Error.pp)
    (load_from_string "");
  [%expect {| ("While reading Herd input from" "(stdin)" "Herd file was empty.") |}]
;;
