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

module type Basic = sig
  type ast (** Raw AST *)
  type t   (** Validated AST *)
  type del (** Delitmusified AST *)

  module Frontend : Lib.Frontend.S with type ast := ast

  val normal_tmp_file_ext : string

  val process : ast -> t Or_error.t

  include Pretty_printer.S with type t := t

  val delitmus : t -> del Or_error.t
  val pp_del : del Fmt.t
end


type mode =
  | Print
  | Delitmus
;;

module Make (B : Basic)
  : Filter.S with type aux_i = mode and type aux_o = unit =
  Filter.Make (struct
    type aux_i = mode
    type aux_o = unit

    let tmp_file_ext ({ aux; _ } : mode Filter.ctx) : string =
      match aux with
      | Print -> B.normal_tmp_file_ext
      | Delitmus -> "c"
    ;;

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

    let run { Filter.aux; src; _ } ic oc = match aux with
      | Print -> run_print src ic oc
      | Delitmus -> run_delitmus src ic oc
    ;;
  end)

module Normal_C : Filter.S with type aux_i = mode and type aux_o = unit =
  Make (struct
    type ast = Ast.Translation_unit.t
    type t = Mini.Program.t
    type del = Nothing.t (* Can't delitmus a C file *)

    let normal_tmp_file_ext = "c"

    module Frontend = Frontend.Normal
    let pp = Fmt.using Mini.Reify.program Ast.Translation_unit.pp
    let process = Mini.Convert.translation_unit

    let delitmus (_ : t) : del Or_error.t =
      Or_error.error_string "Can't delitmus a normal C file"
    ;;

    let pp_del _ (x : del) : unit = Nothing.unreachable_code x
  end)
;;

module Litmus : Filter.S with type aux_i = mode and type aux_o = unit =
  Make (struct
    type ast = Ast.Litmus.t
    type t = Mini.Litmus_ast.Validated.t
    type del = Mini.Program.t

    let normal_tmp_file_ext = "litmus"

    module Frontend = Frontend.Litmus
    let pp = Mini.Litmus_ast.pp
    let process lit =
      Or_error.(
        lit
        |>  Ast.Litmus.validate
        >>= Mini.Convert.litmus
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

    let pp_del : Mini.Program.t Fmt.t =
      Fmt.(prefix pp_prelude
             (using Mini.Reify.program
                (vbox Ast.Translation_unit.pp)))
    ;;

    let delitmus = Delitmus.run
  end)
;;

let c_module (is_c : bool)
  : (module Filter.S with type aux_i = mode and type aux_o = unit) =
  if is_c then (module Normal_C) else (module Litmus)
;;
