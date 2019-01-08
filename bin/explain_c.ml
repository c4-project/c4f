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

open Core
open Utils

type mode =
  | Print
  | Delitmus
;;

module type Basic = sig
  type ast (** Raw AST *)
  type t   (** Validated AST *)
  type del (** Delitmusified AST *)

  module Frontend : Lib.Frontend.S with type ast := ast

  val process : ast -> t Or_error.t

  include Pretty_printer.S with type t := t

  val delitmus : t -> del Or_error.t
  val pp_del : del Fmt.t
end

module Make (B : Basic)
  : Filter.S with type aux_i = mode and type aux_o = unit =
  Filter.Make (struct
    type aux_i = mode
    type aux_o = unit

    let run_delitmus
        (is : Io.In_source.t) (ic : In_channel.t) (oc : Out_channel.t) =
      Or_error.(
        B.Frontend.load_from_ic ~path:(Io.In_source.to_string is) ic
        >>= B.process
        >>= B.delitmus
        >>| Fmt.pf (Format.formatter_of_out_channel oc) "%a@." B.pp_del
      )
    ;;

    let run_print
        (is : Io.In_source.t) (ic : In_channel.t) (oc : Out_channel.t) =
      Or_error.(
        B.Frontend.load_from_ic ~path:(Io.In_source.to_string is) ic
        >>= B.process
        >>| Fmt.pf (Format.formatter_of_out_channel oc) "%a@." B.pp
      )
    ;;

    let run mode is ic _ oc = match mode with
      | Print -> run_print is ic oc
      | Delitmus -> run_delitmus is ic oc
    ;;
  end)

module Normal_C : Filter.S with type aux_i = mode and type aux_o = unit =
  Make (struct
    type ast = C.Ast.Translation_unit.t
    type t = C.Mini.Program.t
    type del = Nothing.t (* Can't delitmus a C file *)

    module Frontend = C.Frontend.Normal
    let pp = Fmt.using C.Mini.Reify.program C.Ast.Translation_unit.pp
    let process = C.Mini.Convert.translation_unit

    let delitmus (_ : t) : del Or_error.t =
      Or_error.error_string "Can't delitmus a normal C file"
    ;;

    let pp_del _ (x : del) : unit = Nothing.unreachable_code x
  end)

module Litmus : Filter.S with type aux_i = mode and type aux_o = unit =
  Make (struct
    type ast = C.Ast.Litmus.t
    type t = C.Mini.Litmus_ast.Validated.t
    type del = C.Mini.Program.t

    module Frontend = C.Frontend.Litmus
    let pp = C.Mini.Litmus_ast.pp
    let process lit =
      Or_error.(
        lit
        |>  C.Ast.Litmus.validate
        >>= C.Mini.Convert.litmus
      )
    ;;

    let prelude : string list =
      [ "// <!> Auto-generated from a litmus test by act."
      ; "#include <stdatomic.h>"
      ; ""
      ]

    let pp_prelude : unit Fmt.t =
      Fmt.(const (vbox (list ~sep:sp string)) prelude)
    ;;

    let pp_del : C.Mini.Program.t Fmt.t =
      Fmt.(prefix pp_prelude
             (using C.Mini.Reify.program
                (vbox C.Ast.Translation_unit.pp)))
    ;;

    let delitmus = C.Delitmus.run
  end)

let c_module is_c
  : (module Filter.S with type aux_i = mode and type aux_o = unit) =
  if is_c then (module Normal_C) else (module Litmus)
;;

let run_delitmus ~(infile_raw : string option) ~(outfile_raw : string option) _o _cfg =
  Litmus.run_from_string_paths Delitmus ~infile:infile_raw ~outfile:outfile_raw
;;

let delitmus_command : Command.t =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"converts a C litmus test to a normal C file"
    [%map_open
      let standard_args = Standard_args.get
      and outfile_raw =
        flag "output"
          (optional file)
          ~doc: "FILE the output file (default: stdout)"
      and infile_raw = anon (maybe ("FILE" %: file)) in
      fun () ->
        Common.lift_command standard_args
          ~with_compiler_tests:false
          ~f:(run_delitmus ~infile_raw ~outfile_raw)
    ]
;;

let run file_type ~(infile_raw : string option) ~(outfile_raw : string option) _o cfg =
  let open Or_error.Let_syntax in
  let%bind infile  = Io.fpath_of_string_option infile_raw in
  let%bind outfile = Io.fpath_of_string_option outfile_raw in
  let      is_c    = Common.decide_if_c infile file_type in
  let      cpp_cfg =
    Option.value (Lib.Config.M.cpp cfg) ~default:(Lib.Cpp.Config.default ())
  in
  let (module M)   = c_module is_c in
  let module Cpp_M = Lib.Cpp.Chain_filter (M) in
  let%map (_, ()) =
    Cpp_M.run_from_fpaths (cpp_cfg, Print) ~infile ~outfile
  in ()
;;

let explain_command : Command.t =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"explains act's understanding of a C file"
    [%map_open
      let standard_args = Standard_args.get
      and outfile_raw =
        flag "output"
          (optional file)
          ~doc: "FILE the explanation output file (default: stdout)"
      and file_type =
        choose_one
          [ Standard_args.Other.flag_to_enum_choice `C "c"
              ~doc:"if given, assume input is raw C"
          ; Standard_args.Other.flag_to_enum_choice `Litmus "litmus"
              ~doc:"if given, assume input is a C litmus test"
          ]
          ~if_nothing_chosen:(`Default_to `Infer)
      and infile_raw = anon (maybe ("FILE" %: file)) in
      fun () ->
        Common.lift_command standard_args
          ~with_compiler_tests:false
          ~f:(run file_type ~infile_raw ~outfile_raw)
    ]
;;

let command : Command.t =
  Command.group
    ~summary:"Commands for dealing with C files"
    [ "delitmus", delitmus_command
    ; "explain", explain_command
    ]
;;
