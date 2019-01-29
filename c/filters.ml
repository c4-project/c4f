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

  val cvars : t -> string list option
  (** [cvars vast] should return a list of C identifiers
      corresponding to all variables in [vast], if possible.

      [cvars] _may_ de-litmusify the AST in the process;
      if you already have a delitmusified AST, use
      [cvars_of_delitmus]. *)

  val cvars_of_delitmus : del -> string list option
  (** [cvars_of_delitmus dl] should return a list of C identifiers
      corresponding to all variables in [dl], if possible. *)

  val postcondition : t -> Mini.Litmus_ast.Post.t option
  (** [postcondition vast] should get the Litmus postcondition of
     [vast], if one exists. *)

  include Pretty_printer.S with type t := t

  val delitmus : t -> del Or_error.t
  val pp_del : del Fmt.t
end

type mode =
  | Print of [ `All | `Vars ]
  | Delitmus
;;

module Output = struct
  type t =
    { cvars : string list option
    ; post  : Mini.Litmus_ast.Post.t option
    }
  [@@deriving fields]
end

module Make (B : Basic)
  : Filter.S with type aux_i = mode and type aux_o = Output.t =
  Filter.Make (struct
    type aux_i = mode
    type aux_o = Output.t
    let name = "C transformer"

    let tmp_file_ext ({ aux; _ } : mode Filter.ctx) : string =
      match aux with
      | Print `All -> B.normal_tmp_file_ext
      | Print `Vars -> "txt"
      | Delitmus -> "c"
    ;;

    let run_delitmus (vast : B.t) (oc : Out_channel.t)
      : Output.t Or_error.t =
      let open Or_error.Let_syntax in
      let%map dl = B.delitmus vast in
      Fmt.pf (Format.formatter_of_out_channel oc) "%a@." B.pp_del dl;
      let cvars = B.cvars_of_delitmus dl in
      let post  = B.postcondition vast in
      { Output.cvars; post }
    ;;

    let pp : [ `All | `Vars ] -> (string list * B.t) Fmt.t = function
      | `All -> Fmt.using snd B.pp
      | `Vars -> Fmt.(using fst (vbox (list ~sep:sp string)))
    ;;

    let run_print
        (output_mode : [ `All | `Vars ])
        (vast : B.t) (oc : Out_channel.t)
      : Output.t Or_error.t =
      let cvars = B.cvars vast in
      let post  = B.postcondition vast in
      let f = Format.formatter_of_out_channel oc in
      Fmt.pf f "%a@." (pp output_mode)
        (Option.value ~default:[] cvars, vast);
      Or_error.return { Output.cvars; post }
    ;;

    let run { Filter.aux; src; _ } ic oc : Output.t Or_error.t =
      let open Or_error.Let_syntax in
      let%bind ast =
        B.Frontend.load_from_ic ~path:(Io.In_source.to_string src) ic
      in
      let%bind vast = B.process ast in
      match aux with
      | Print output_mode -> run_print output_mode vast oc
      | Delitmus -> run_delitmus vast oc
    ;;
  end)


let cvars_of_program (prog : Mini.Program.t) : string list option =
  let names =
    prog
    |> Mini.Program.On_decls.to_list
    |> List.map ~f:(fun (x, _) -> C_identifier.to_string x)
  in Some names
;;

module Normal_C : Filter.S with type aux_i = mode and type aux_o = Output.t =
  Make (struct
    type ast = Ast.Translation_unit.t
    type t = Mini.Program.t
    type del = Nothing.t (* Can't delitmus a C file *)

    let normal_tmp_file_ext = "c"

    module Frontend = Frontend.Normal
    let pp = Fmt.using Mini.Reify.program Ast.Translation_unit.pp
    let process = Mini_convert.translation_unit

    let cvars = cvars_of_program
    let cvars_of_delitmus = Nothing.unreachable_code
    let postcondition = Fn.const None

    let delitmus (_ : t) : del Or_error.t =
      Or_error.error_string "Can't delitmus a normal C file"
    ;;

    let pp_del _ (x : del) : unit = Nothing.unreachable_code x
  end)
;;

module Litmus : Filter.S with type aux_i = mode and type aux_o = Output.t =
  Make (struct
    type ast = Ast.Litmus.t
    type t = Mini.Litmus_ast.Validated.t
    type del = Mini.Program.t

    let normal_tmp_file_ext = "litmus"

    module Frontend = Frontend.Litmus
    let pp = Mini.Litmus_pp.pp
    let process lit =
      Or_error.(
        lit
        |>  Ast.Litmus.validate
        >>= Mini_convert.litmus
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

    let postcondition
      : Mini.Litmus_ast.Validated.t -> Mini.Litmus_ast.Post.t option =
      Mini.Litmus_ast.Validated.post

    let cvars_of_delitmus = cvars_of_program

    let cvars (lit : t) : string list option =
        Option.(Result.ok (delitmus lit) >>= cvars_of_delitmus)
    ;;
  end)
;;

let c_module (is_c : bool)
  : (module Filter.S with type aux_i = mode and type aux_o = Output.t) =
  if is_c then (module Normal_C) else (module Litmus)
;;
