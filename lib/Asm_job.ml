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

open Core
open Utils

include Asm_job_intf

type 'fmt t =
  { format  : 'fmt option
  ; passes  : Sanitiser_pass.Set.t [@default Sanitiser_pass.standard]
  ; symbols : string list
  }
[@@deriving make]
;;

module Litmus_format = struct
  type t =
    | Full
    | Programs_only
  [@@deriving eq]
  ;;
  let default = Full
end

module Explain_format = struct
  type t =
    | Assembly
    | Detailed
  [@@deriving eq]
  ;;
  let default = Assembly
end

(** [output] is the output of a single-file job. *)
type output =
  { symbol_map : (string, string) List.Assoc.t
  ; warn       : Format.formatter -> unit
  } [@@deriving fields]
;;

module type Runner =
  Gen_runner with type 'fmt inp := 'fmt t
              and type aux := output
              and type lfmt := Litmus_format.t
              and type efmt := Explain_format.t
;;

module Make_runner (B : Runner_deps) : Runner = struct
  (* Shorthand for modules we use a _lot_. *)
  module L  = B.Litmus_ast
  module LP = B.Litmus_pp
  module LS = B.Src_lang
  module LD = B.Dst_lang
  module MS = B.Multi_sanitiser
  module SS = B.Single_sanitiser
  module E  = B.Explainer

  let parse isrc inp =
    let iname = Io.In_source.to_string isrc in
    Or_error.tag_arg
      (B.Frontend.load_from_ic ~path:iname inp)
      "Error while parsing assembly" iname String.sexp_of_t
  ;;

  let make_init (progs : MS.Output.Program.t list)
    : (string, LD.Constant.t) List.Assoc.t =
    let get_hsyms prog =
      prog
      |> MS.Output.Program.symbol_table
      |> Fn.flip Abstract.Symbol.Table.set_of_sort Abstract.Symbol.Sort.Heap
    in
    let syms = Abstract.Symbol.Set.union_list (List.map ~f:get_hsyms progs) in
    List.map ~f:(fun s -> (s, LD.Constant.zero))
      (Abstract.Symbol.Set.to_list syms)
  ;;

  let stringify_redirects =
    List.map
      ~f:(fun (k, v) -> (LS.Symbol.to_string k, LS.Symbol.to_string v))
  ;;

  let emit_warnings iname = function
    | [] -> Fn.const ()
    | ws ->
      let pp_warning f w =
        Format.fprintf f "@[<h>-@ @[<hov>%a@]@]@,"
          MS.Warn.pp w
      in
      fun f ->
        Format.fprintf f "Warnings@ for@ %s:@ @[<v>%a@]@."
          iname
          (fun f -> List.iter ~f:(pp_warning f)) ws
  ;;

  let make_output iname redirects warnings : output =
    { symbol_map = stringify_redirects redirects
    ; warn       = emit_warnings iname warnings
    }
  ;;

  let pp_for_litmus_format
    : Litmus_format.t -> Base.Formatter.t -> L.Validated.t -> unit = function
    | Full          -> LP.pp
    | Programs_only -> LP.pp_programs
  ;;

  let make_litmus_programs (programs : MS.Output.Program.t list) =
    List.map programs
      ~f:(fun program ->
          program |> MS.Output.Program.listing |> B.final_convert)
  ;;

  let make_litmus name (programs : MS.Output.Program.t list) =
    Or_error.tag ~tag:"Couldn't build litmus file."
      ( L.Validated.make
          ~name
          ~init:(make_init programs)
          ~programs:(make_litmus_programs programs)
          ()
      )
  ;;

  let collate_warnings (programs : MS.Output.Program.t list) =
    List.concat_map programs ~f:(MS.Output.Program.warnings)
  ;;

  let output_litmus
      ?(output_format : Litmus_format.t = Litmus_format.default)
      (name : string)
      (passes : Sanitiser_pass.Set.t)
      (symbols : LS.Symbol.t list)
      (program : LS.Program.t)
      (_osrc : Io.Out_sink.t)
      (outp : Out_channel.t)
    : output Or_error.t =
    let open Or_error.Let_syntax in
    let%bind o = MS.sanitise ~passes ~symbols program in
    let programs = MS.Output.programs o in
    let warnings = collate_warnings programs in
    let%map lit = make_litmus name programs in
    let f = Format.formatter_of_out_channel outp in
    pp_for_litmus_format output_format f lit;
    Format.pp_print_newline f ();
    make_output name (MS.Output.redirects o) warnings
  ;;

  let pp_for_explain_format
    : Explain_format.t -> Base.Formatter.t -> E.t -> unit = function
    | Assembly -> E.pp_as_assembly
    | Detailed -> E.pp
  ;;

  let output_explanation output_format name outp exp redirects =
    let f = Format.formatter_of_out_channel outp in
    pp_for_explain_format output_format f exp;
    Format.pp_print_newline f ();
    make_output name redirects []
  ;;

  let run_explanation
      ?(output_format : Explain_format.t = Explain_format.default)
      (name    : string)
      (passes  : Sanitiser_pass.Set.t)
      (symbols : LS.Symbol.t list)
      (program : LS.Program.t)
      (_osrc   : Io.Out_sink.t)
      (outp    : Out_channel.t)
    : output Or_error.t =
    let open Or_error.Let_syntax in
    let%map san = SS.sanitise ~passes ~symbols program in
    let program = SS.Output.programs san in
    let listing = SS.Output.Program.listing program in
    let s_table = SS.Output.Program.symbol_table program in
    let exp = E.explain listing s_table in
    let redirects = SS.Output.redirects san in
    output_explanation output_format name outp exp redirects
  ;;

  let stringify_symbol sym =
    Result.of_option (LS.Symbol.of_string_opt sym)
      ~error:(
        Error.create_s
          [%message "Symbol can't be converted from string"
              ~symbol:sym]
      )
  ;;

  let stringify_symbols syms =
    syms
    |> List.map ~f:stringify_symbol
    |> Or_error.combine_errors
  ;;

  let run ~f { Filter.aux; src; sink } ic oc : output Or_error.t =
    let open Result.Let_syntax in
    let name = Filename.basename (Io.In_source.to_string src) in
    let%bind asm = parse src ic in
    let%bind symbols = stringify_symbols aux.symbols in
    f ?output_format:aux.format name aux.passes symbols (B.program asm) sink oc
  ;;

  module Litmusify : Filter.S with type aux_i = Litmus_format.t t
                               and type aux_o = output =
    Filter.Make (struct
      type aux_i = Litmus_format.t t
      type aux_o = output
      let tmp_file_ext = Fn.const "litmus"

      let run = run ~f:output_litmus
    end)
  ;;

  module Explain : Filter.S with type aux_i = Explain_format.t t
                             and type aux_o = output =
    Filter.Make (struct
      type aux_i = Explain_format.t t
      type aux_o = output
      let tmp_file_ext = Fn.const "s"

      let run = run ~f:run_explanation
    end)
  ;;
end

let get_litmusify (module Runner : Runner)
  : ( module Filter.S with type aux_i = Litmus_format.t t
                       and type aux_o = output
    ) = (module Runner.Litmusify)
;;

let get_explain (module Runner : Runner)
  : ( module Filter.S with type aux_i = Explain_format.t t
                       and type aux_o = output
    ) = (module Runner.Explain)
;;
