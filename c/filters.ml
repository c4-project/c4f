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
  type ast
  (** Raw AST *)

  type t
  (** Validated AST *)

  type del
  (** Delitmusified AST *)

  module Frontend : Lib.Frontend.S with type ast := ast

  val normal_tmp_file_ext : string

  val process : ast -> t Or_error.t

  val fuzz : seed:int option -> o:Lib.Output.t -> t -> t Or_error.t

  val cvars : t -> String.Set.t
  (** [cvars vast] should return a list of C identifiers
      corresponding to all variables in [vast].

      [cvars] _may_ de-litmusify the AST in the process;
      if you already have a delitmusified AST, use
      [cvars_of_delitmus]. *)

  val cvars_of_delitmus : del -> String.Set.t
  (** [cvars_of_delitmus dl] should return a list of C identifiers
      corresponding to all variables in [dl]. *)

  val postcondition : t -> Mini_litmus.Ast.Post.t option
  (** [postcondition vast] should get the Litmus postcondition of
     [vast], if one exists. *)

  include Pretty_printer.S with type t := t

  val delitmus : t -> del Or_error.t
  val pp_del : del Fmt.t
end

type mode =
  | Print of [ `All | `Vars ]
  | Delitmus
  | Fuzz of { seed : int option; o : Lib.Output.t }
;;

module Output = struct
  type t =
    { cvars : String.Set.t
    ; post  : Mini_litmus.Ast.Post.t option
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
      | Fuzz _ -> "c.litmus"
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

    let run_fuzz ~(seed : int option) ~(o : Lib.Output.t) (vast : B.t) (oc : Out_channel.t)
      : Output.t Or_error.t =
      let open Or_error.Let_syntax in
      let%map fz = B.fuzz ~seed ~o vast in
      Fmt.pf (Format.formatter_of_out_channel oc) "%a@." B.pp fz;
      let cvars = B.cvars vast in
      let post  = B.postcondition vast in
      { Output.cvars; post }

    let pp : [ `All | `Vars ] -> (String.Set.t * B.t) Fmt.t = function
      | `All -> Fmt.using snd B.pp
      | `Vars -> Fmt.(using fst (vbox (using String.Set.to_list (list ~sep:sp string))))
    ;;

    let run_print
        (output_mode : [ `All | `Vars ])
        (vast : B.t) (oc : Out_channel.t)
      : Output.t Or_error.t =
      let cvars = B.cvars vast in
      let post  = B.postcondition vast in
      let f = Format.formatter_of_out_channel oc in
      Fmt.pf f "%a@." (pp output_mode) (cvars, vast);
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
      | Fuzz { seed; o } -> run_fuzz ~seed ~o vast oc
    ;;
  end)

module Normal_C : Filter.S with type aux_i = mode and type aux_o = Output.t =
  Make (struct
    type ast = Ast.Translation_unit.t
    type t = Mini.Program.t
    type del = Nothing.t (* Can't delitmus a C file *)

    let normal_tmp_file_ext = "c"

    module Frontend = Frontend.Normal
    let pp = Fmt.using Mini_reify.program Ast.Translation_unit.pp
    let process = Mini_convert.translation_unit

    let fuzz ~(seed : int option) ~(o : Lib.Output.t) (_ : t)
      : t Or_error.t =
      ignore seed;
      ignore o;
      Or_error.error_string "Can't fuzz a normal C file"
    ;;

    let cvars prog =
      prog
      |> Mini.Program.cvars
      |> String.Set.map ~f:C_identifier.to_string
    ;;

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
    type t = Mini_litmus.Ast.Validated.t
    type del = Mini.Program.t

    let normal_tmp_file_ext = "litmus"

    module Frontend = Frontend.Litmus
    let pp = Mini_litmus.Pp.pp
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
             (using Mini_reify.program
                (vbox Ast.Translation_unit.pp)))
    ;;

    let delitmus = Delitmus.run

    let fuzz : seed:int option -> o:Lib.Output.t -> t -> t Or_error.t = Fuzzer.run

    let postcondition
      : Mini_litmus.Ast.Validated.t -> Mini_litmus.Ast.Post.t option =
      Mini_litmus.Ast.Validated.post

    let cvars_of_delitmus prog =
      prog
      |> Mini.Program.cvars
      |> String.Set.map ~f:C_identifier.to_string
    ;;

    let cvars prog =
      prog
      |> Mini_litmus.cvars
      |> String.Set.map ~f:C_identifier.to_string
    ;;
  end)
;;

let c_module (is_c : bool)
  : (module Filter.S with type aux_i = mode and type aux_o = Output.t) =
  if is_c then (module Normal_C) else (module Litmus)
;;
