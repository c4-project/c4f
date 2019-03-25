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

open Core_kernel
open Utils
open Lib

(** Represents an observation that the C and assembly sets of a
    state-set-level analysis aren't equal. *)
module State_deviation = struct
  type t =
    { in_c_only : Herd_output.State.t list
    ; in_asm_only : Herd_output.State.t list
    }
  [@@deriving sexp_of, fields, make]

  let of_order_opt : Herd_output.State.Set.Partial_order.t -> t option =
    (* C is left, asm is right. *)
    function
    | Equal -> None
    | Subset { in_right_only } -> Some (make ~in_asm_only:(Set.to_list in_right_only) ())
    | Superset { in_left_only } -> Some (make ~in_c_only:(Set.to_list in_left_only) ())
    | No_order { in_left_only; in_right_only } ->
      Some
        (make
           ~in_c_only:(Set.to_list in_left_only)
           ~in_asm_only:(Set.to_list in_right_only)
           ())
  ;;

  let of_herd_outcome_opt : Herd_output.outcome -> t option = function
    | `Order o -> of_order_opt o
    | `Unknown | `Undef | `OracleUndef -> None
  ;;
end

module Herd = struct
  type t =
    [ Herd_output.outcome
    | `Disabled
    | `Errored of [ `C | `Assembly ]
    ]
  [@@deriving sexp_of]

  let order_operator : Herd_output.State.Set.Partial_order.t -> string = function
    | Equal -> "=="
    | Subset _ -> "<<"
    | Superset _ -> ">>"
    | No_order _ -> "<>"
  ;;

  let to_string : t -> string = function
    | `Errored `Assembly -> "ERROR (asm)"
    | `Errored `C -> "ERROR (C)"
    | `Disabled -> "--disabled--"
    | `Unknown -> "??"
    | `Undef -> "UNDEFINED BEHAVIOUR (asm)"
    | `OracleUndef -> "UNDEFINED BEHAVIOUR (C)"
    | `Order o -> sprintf "C %s asm" (order_operator o)
  ;;

  let pp : t Fmt.t = Fmt.of_to_string to_string

  let to_state_deviation_opt : t -> State_deviation.t option = function
    | #Herd_output.outcome as o -> State_deviation.of_herd_outcome_opt o
    | `Errored _ | `Disabled -> None
  ;;
end

module File = struct
  type t =
    { time_taken : Time.Span.t option
    ; time_taken_in_cc : Time.Span.t option
    ; herd : Herd.t
    }
  [@@deriving sexp_of, fields, make]
end

module Compiler = struct
  type t =
    { time_taken : Time.Span.t option
    ; files : (string, File.t) List.Assoc.t
    }
  [@@deriving sexp_of, fields, make]
end

module Machine = struct
  type t =
    { time_taken : Time.Span.t option
    ; compilers : (Config.Id.t, Compiler.t) List.Assoc.t
    }
  [@@deriving sexp_of, fields, make]

  let files m =
    List.concat_map (compilers m) ~f:(fun (cid, compiler) ->
        List.map (Compiler.files compiler) ~f:(fun (fname, analysis) ->
            cid, fname, analysis))
  ;;
end

module Row = struct
  type 'a t =
    { machine_id : Config.Id.t
    ; compiler_id : Config.Id.t
    ; filename : string
    ; analysis : 'a
    }
  [@@deriving sexp_of, fields, make]

  let pp_span_opt f = function
    | Some span -> Time.Span.pp f span
    | None -> String.pp f "-"
  ;;

  let to_table_row (row : File.t t) : Tabulator.row =
    [ Fn.flip Config.Id.pp row.machine_id
    ; Fn.flip Config.Id.pp row.compiler_id
    ; Fn.flip String.pp row.filename
    ]
    @ (* We use Fieldslib here, mainly, to raise compilation errors
       if the fields in [analysis] change and we don't add them
       (or ignore them) here. *)
      File.Fields.Direct.to_list
        row.analysis
        ~herd:(fun _field _file h f -> Herd.pp f h)
        ~time_taken:(fun _field _file t f -> pp_span_opt f t)
        ~time_taken_in_cc:(fun _field _file t f -> pp_span_opt f t)
  ;;

  let with_analysis (row : _ t) (analysis : 'a) : 'a t =
    make
      ~machine_id:(machine_id row)
      ~compiler_id:(compiler_id row)
      ~filename:(filename row)
      ~analysis
  ;;

  let deviations (row : File.t t) : State_deviation.t t option =
    let open Option.Let_syntax in
    let file = analysis row in
    let herd = File.herd file in
    let%map deviations = Herd.to_state_deviation_opt herd in
    with_analysis row deviations
  ;;
end

module M = struct
  type t =
    { time_taken : Time.Span.t option
    ; machines : (Config.Id.t, Machine.t) List.Assoc.t
    }
  [@@deriving sexp_of, fields, make]

  let file_rows (a : t) : File.t Row.t list =
    List.concat_map (machines a) ~f:(fun (machine_id, machine) ->
        List.map (Machine.files machine) ~f:(fun (compiler_id, filename, analysis) ->
            Row.make ~machine_id ~compiler_id ~filename ~analysis))
  ;;

  let deviation_rows (a : t) : State_deviation.t Row.t list =
    a |> file_rows |> List.filter_map ~f:Row.deviations
  ;;

  let machine_rule = '='
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
      if Config.Id.equal lmid this_mid
      then
        if Config.Id.equal lcid this_cid
        then Or_error.return tabulator (* no rule *)
        else Tabulator.with_rule compiler_rule tabulator
      else Tabulator.with_rule machine_rule tabulator
  ;;

  let results_table_names : string list Lazy.t =
    lazy
      ([ "Machine"; "Compiler"; "File" ]
      @ File.Fields.to_list
          ~herd:(fun _ -> "Result")
          ~time_taken:(fun _ -> "Time/total")
          ~time_taken_in_cc:(fun _ -> "Time/CC"))
  ;;

  let results_table_header =
    lazy (List.map (force results_table_names) ~f:(Fn.flip String.pp))
  ;;

  let with_file (last_mid, last_cid, tabulator)
                (row : File.t Row.t) =
    let mid = Row.machine_id row in
    let cid = Row.compiler_id row in
    Or_error.(
      return tabulator
      >>= maybe_with_rule last_mid mid last_cid cid
      >>= Tabulator.with_row (Row.to_table_row row)
      >>| fun t' -> Some mid, Some cid, t')
  ;;

  type data = t (* For compatibility with Extend_tabular *)

  let to_table a =
    let header = force results_table_header in
    Tabulator.(
      let open Or_error.Let_syntax in
      let%bind t = make ~header () in
      let%map _, _, t =
        Travesty.T_list.With_errors.fold_m
          (file_rows a)
          ~init:(None, None, t)
          ~f:with_file
      in
      t)
  ;;
end

include M
include Tabulator.Extend_tabular (M)
