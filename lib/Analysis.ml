(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

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

(** [Analysis] contains abstract data types for storing finished
   analyses of programs. *)

open Core
open Utils

module Herd = struct
  type t =
    [ Herd_output.outcome
    | `Disabled
    | `Errored of [`C | `Assembly]
    ] [@@deriving sexp_of]
  ;;

  let pp f : t -> unit = function
    | `Errored `Assembly -> String.pp f "ERROR (asm)"
    | `Errored `C        -> String.pp f "ERROR (C)"
    | `Disabled          -> String.pp f "--disabled--"
    | `Unknown           -> String.pp f "??"
    | `Undef             -> String.pp f "UNDEFINED BEHAVIOUR (asm)"
    | `OracleUndef       -> String.pp f "UNDEFINED BEHAVIOUR (C)"
    | `Equal             -> String.pp f "C == asm"
    | `Subset   _        -> String.pp f "C << asm"
    | `Superset _        -> String.pp f "C >> asm"
    | `NoOrder           -> String.pp f "C <> asm"
  ;;
end

module File = struct
  type t =
    { time_taken       : Time.Span.t option
    ; time_taken_in_cc : Time.Span.t option
    ; herd             : Herd.t
    } [@@deriving sexp_of, fields, make]
end

module Compiler = struct
  type t =
    { time_taken : Time.Span.t option
    ; files      : (string, File.t) List.Assoc.t
    } [@@deriving sexp_of, fields, make]
end

module Machine = struct
  type t =
    { time_taken : Time.Span.t option
    ; compilers  : (Id.t, Compiler.t) List.Assoc.t
    } [@@deriving sexp_of, fields, make]
  ;;

  let files m =
    List.concat_map (compilers m)
      ~f:(fun (cid, compiler) ->
          List.map (Compiler.files compiler)
            ~f:(fun (fname, analysis) ->
                (cid, fname, analysis)))
  ;;
end

module M = struct
  type t =
    { time_taken : Time.Span.t option
    ; machines   : (Id.t, Machine.t) List.Assoc.t
    } [@@deriving sexp_of, fields, make]
  ;;

  let files a =
    List.concat_map (machines a)
      ~f:(fun (mid, machine) ->
          List.map (Machine.files machine)
            ~f:(fun (cid, fname, analysis) ->
                (mid, cid, fname, analysis)))
  ;;

  let machine_rule  = '='
  let compiler_rule = '-'

  (** [maybe_with_rule last_mid this_mid last_cid this_cid tabulator]
      determines, by checking whether the machine or compiler IDs have
      changed from last row (if there was a last row), whether to
      insert a rule on [tabulator] and, if so, which one to insert. *)
  let maybe_with_rule last_mid this_mid last_cid this_cid tabulator =
    match last_mid, last_cid with
    | None, _ | _, None ->
      (* Assume we're on the first row *)
      Tabulator.with_rule machine_rule tabulator
    | Some lmid, Some lcid ->
      if Id.equal lmid this_mid
      then (
        if Id.equal lcid this_cid
        then Or_error.return tabulator (* no rule *)
        else Tabulator.with_rule compiler_rule tabulator
      )
      else Tabulator.with_rule machine_rule tabulator
  ;;

  let pp_span_opt f = function
    | Some span -> Time.Span.pp f span
    | None      -> String.pp f "-"
  ;;

  let results_table_names : string list Lazy.t =
    lazy (
      [ "Machine"
      ; "Compiler"
      ; "File"
      ]
      @
      File.Fields.to_list
        ~herd:(fun _ -> "Result")
        ~time_taken:(fun _ -> "Time/total")
        ~time_taken_in_cc:(fun _ -> "Time/CC")
    )
  ;;

  let results_table_header =
    lazy (
      List.map (force results_table_names)
        ~f:(Fn.flip String.pp)
    )
  ;;

  let file_to_row mid cid file analysis =
    [ Fn.flip Id.pp mid
    ; Fn.flip Id.pp cid
    ; Fn.flip String.pp file
    ] @
    (* We use Fieldslib here, mainly, to raise compilation errors
       if the fields in [analysis] change and we don't add them
       (or ignore them) here. *)
    File.Fields.Direct.to_list analysis
      ~herd:(fun _field _file h f -> Herd.pp f h)
      ~time_taken:(fun _field _file t f -> pp_span_opt f t)
      ~time_taken_in_cc:(fun _field _file t f -> pp_span_opt f t)
  ;;

  let with_file
      (last_mid, last_cid, tabulator)
      (mid, cid, file, analysis) =
    let open Or_error in
    return tabulator
    >>= maybe_with_rule last_mid mid last_cid cid
    >>= Tabulator.with_row (file_to_row mid cid file analysis)
    >>| (fun t' -> Some mid, Some cid, t')
  ;;

  type data = t (* For compatibility with Extend_tabular *)

  let to_table a =
    let header = force results_table_header in
    Tabulator.(
      let open Or_error.Let_syntax in
      let%bind t = make ~header () in
      let%map (_, _, t) =
        My_list.With_errors.foldM (files a)
          ~init:(None, None, t) ~f:with_file
      in
      t
    )
    ;;
end

include M
include Tabulator.Extend_tabular (M)
