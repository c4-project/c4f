(* This file is part of 'act'.

Copyright (c) 2018 by Matt Windsor

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. *)

(** Parsing and comparison functionality for Herd output

[Herd] contains functions for scraping the human-readable summary of a
   Herd7 run, extracting location information, and comparing states.
   *)

open Core
open Utils

type state = (string, string) List.Assoc.t;;

type t =
  { states : state list
  }
;;

(** [init ()] generates an initial [t]. *)
let init () =
  { states = []
  }

type single_outcome =
  [ `Unknown
  | `Undef
  ]
[@@deriving sexp]
;;

type outcome =
  [ single_outcome
  | `OracleUndef
  | `Equal
  | `Subset
  | `Superset
  | `NoOrder
  ]
[@@deriving sexp]
;;

let single_outcome_of (_herd : t) : single_outcome =
  `Unknown
;;

(** [rstate] is the current state of a Herd reader. *)
type rstate =
  | Empty    (** We haven't read anything yet. *)
  | Preamble (** We're in the pre-state matter. *)

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

(** [fail_if_empty] produces an error if the reader ended in state
    Empty -- that is, the Herd file was completely empty. *)
let fail_if_empty (r : reader) : unit Or_error.t =
  if r.state = Empty
  then Or_error.error_s [%message "Herd file was empty."]
  else Result.ok_unit
;;

(** [validate r] runs some checks on the result of processing
   a Herd file using reader [r]. *)
let validate (r : reader) : t Or_error.t =
  let open Or_error.Let_syntax in
  Or_error.tag_arg
    begin
      let%bind () = fail_if_empty r in
      return r.herd
    end
    "While reading Herd input from"
    (Option.value ~default:"(stdin)" r.path)
    [%sexp_of: string]
;;

let process_line (herd : reader) (_line : string) : reader =
  let state' = Preamble in
  { herd with state = state' }
;;

module Load : Io.LoadableS with type t = t = struct
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

include Io.LoadableMake (Load);;

let%expect_test "load_from_string on empty string fails" =
  Or_error.iter_error
    ~f:(Format.printf "@[<v>%a@]@." Error.pp)
    (load_from_string "");
  [%expect {| ("While reading Herd input from" "(stdin)" "Herd file was empty.") |}]
;;
