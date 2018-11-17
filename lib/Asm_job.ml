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

open Core
open Utils

type t =
  { inp     : Io.In_source.t
  ; outp    : Io.Out_sink.t
  ; passes  : Sanitiser_pass.Set.t
  ; symbols : string list
  }
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

module type Runner_deps = sig
  type ast

  module Frontend : Frontend.S with type ast := ast
  module Litmus : Litmus.S
  module Multi_sanitiser
    : Sanitiser.S with type statement = Litmus.Lang.Statement.t
                   and type sym = Litmus.Lang.Symbol.t
                   and type 'a Program_container.t = 'a list
  ;;
  module Single_sanitiser
    : Sanitiser.S with type statement = Litmus.Lang.Statement.t
                   and type sym = Litmus.Lang.Symbol.t
                   and type 'a Program_container.t = 'a
  ;;
  module Explainer
    : Explainer.S with type statement := Litmus.Lang.Statement.t
  ;;

  val final_convert : Litmus.Lang.Statement.t list -> Litmus.Lang.Statement.t list

  val statements : ast -> Litmus.Lang.Statement.t list
end

module type Runner = sig
  val litmusify
    :  ?output_format:Litmus_format.t
    -> t
    -> output Or_error.t
  ;;
  val explain
    :  ?output_format:Explain_format.t
    -> t
    -> output Or_error.t
  ;;
end

module Make_runner (B : Runner_deps) : Runner = struct
  (* Shorthand for modules we use a _lot_. *)
  module L  = B.Litmus
  module LS = L.Lang
  module MS = B.Multi_sanitiser
  module SS = B.Single_sanitiser
  module E  = B.Explainer

  let name_of isrc =
    Option.value (Io.In_source.file isrc) ~default:"(stdin)"
  ;;

  let parse isrc inp =
    let iname = name_of isrc in
    Or_error.tag_arg
      (B.Frontend.load_from_ic ~path:iname inp)
      "Error while parsing assembly" iname String.sexp_of_t
  ;;

  let make_init (progs : LS.Statement.t list list) : (string, LS.Constant.t) List.Assoc.t =
    let get_hsyms prog =
      prog
      |> LS.symbols
      |> Fn.flip Abstract.Symbol.Table.set_of_sort Abstract.Symbol.Sort.Heap
    in
    let syms = Abstract.Symbol.Set.union_list (List.map ~f:get_hsyms progs) in
    List.map ~f:(fun s -> (s, LS.Constant.zero))
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
    : Litmus_format.t -> Base.Formatter.t -> L.t -> unit = function
    | Full          -> L.pp
    | Programs_only -> L.pp_programs
  ;;

  let make_litmus name programs =
    Or_error.tag ~tag:"Couldn't build litmus file."
      ( L.make ~name
          ~init:(make_init programs)
          ~programs:(List.map ~f:B.final_convert programs)
      )
  ;;

  let output_litmus
      (output_format : Litmus_format.t)
      (name : string)
      (passes : Sanitiser_pass.Set.t)
      (symbols : LS.Symbol.t list)
      (program : LS.Statement.t list)
      (_osrc : Io.Out_sink.t)
      (outp : Out_channel.t) =
    let open Or_error.Let_syntax in
    let%bind o = MS.sanitise ~passes ~symbols program in
    let programs = MS.Output.result o in
    let warnings = MS.Output.warnings o in
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

  let output_explanation
      (output_format : Explain_format.t)
      (name    : string)
      (passes  : Sanitiser_pass.Set.t)
      (symbols : LS.Symbol.t list)
      (program : LS.Statement.t list)
      (_osrc   : Io.Out_sink.t)
      (outp    : Out_channel.t) =
    let open Or_error.Let_syntax in
    let%map san = SS.sanitise ~passes ~symbols program in
    let exp = E.explain (SS.Output.result san) in
    let f = Format.formatter_of_out_channel outp in
    pp_for_explain_format output_format f exp;
    Format.pp_print_newline f ();
    make_output name (SS.Output.redirects san) []
  ;;

  let stringify_symbols syms =
    syms
    |> List.map
      ~f:(fun s -> Result.of_option (LS.Symbol.of_string_opt s)
             ~error:(
               Error.create_s
                 [%message "Symbol can't be converted from string"
                     ~symbol:s]
             )
         )
    |> Or_error.combine_errors
  ;;

  let run ~f t =
    let open Result.Let_syntax in
    let name = Filename.basename (name_of t.inp) in
    let%bind asm = Io.In_source.with_input ~f:parse t.inp in
    let%bind symbols = stringify_symbols t.symbols in
    Io.Out_sink.with_output t.outp
      ~f:(f name t.passes symbols (B.statements asm))
  ;;

  let litmusify ?(output_format=Litmus_format.default) =
    run ~f:(output_litmus output_format)
  ;;
  let explain ?(output_format=Explain_format.default) =
    run ~f:(output_explanation output_format)
  ;;
end
