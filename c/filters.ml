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

module Var_scope = struct
  module M = struct
    type t =
      | Unknown
      | Local
      | Global
    [@@deriving sexp, equal]
    ;;
  end
  include M

  include Comparable.Make (struct
      (* The comparison scheme used here is very deliberate, hence
         why we write it out explicitly:

         - information about a scope > no information about a scope;
         - considering a variable as global > considering it as local *)

      include M
      let weight = function
        | Unknown -> 0
        | Local   -> 1
        | Global  -> 2
      ;;
      let compare = Travesty.T_fn.on weight Int.compare
    end)

  let brand
      (scope : t)
      (cvars : Utils.C_identifier.Set.t) : t Utils.C_identifier.Map.t =
    Utils.C_identifier.Set.to_map cvars ~f:(Fn.const scope)
  ;;

  let resolve_cvar_clashes ~key =
    ignore key;
    function
    | `Left ty | `Right ty -> Some ty
    | `Both (l, r) -> Some (max l r)
  ;;

  let make_map_opt
      ?(locals : Utils.C_identifier.Set.t option)
      ?(globals : Utils.C_identifier.Set.t option)
      ()
    : t Utils.C_identifier.Map.t option =
    let locals_map  = Option.map ~f:(brand Local ) locals in
    let globals_map = Option.map ~f:(brand Global) globals in
    Option.merge locals_map globals_map
      ~f:(Utils.C_identifier.Map.merge ~f:resolve_cvar_clashes)
  ;;
end

module type Basic = sig
  type ast
  (** Raw AST *)

  type t
  (** Validated AST *)

  type del
  (** Delitmusified AST *)

  module Frontend : Utils.Frontend.S with type ast := ast

  val normal_tmp_file_ext : string

  val process : ast -> t Or_error.t

  val fuzz : seed:int option -> o:Lib.Output.t -> t -> t Or_error.t

  val cvars : t -> Var_scope.t Utils.C_identifier.Map.t
  (** [cvars vast] should return a list of C identifiers
      corresponding to all variables in [vast].

      [cvars] _may_ de-litmusify the AST in the process;
      if you already have a delitmusified AST, use
      [cvars_of_delitmus]. *)

  val cvars_of_delitmus : del -> Var_scope.t Utils.C_identifier.Map.t
  (** [cvars_of_delitmus dl] should return a list of C identifiers
      corresponding to all variables in [dl]. *)

  val postcondition : t -> Mini_litmus.Ast.Postcondition.t option
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
    { cvars : Var_scope.t Utils.C_identifier.Map.t
    ; post  : Mini_litmus.Ast.Postcondition.t option
    }
  [@@deriving fields]
end

module Make (B : Basic)
  : Utils.Filter.S with type aux_i = mode and type aux_o = Output.t =
  Utils.Filter.Make (struct
    type aux_i = mode
    type aux_o = Output.t
    let name = "C transformer"

    let tmp_file_ext ({ aux; _ } : mode Utils.Filter.ctx) : string =
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

    let pp
      : [ `All | `Vars ] -> (Utils.C_identifier.t list * B.t) Fmt.t = function
      | `All -> Fmt.using snd B.pp
      | `Vars -> Fmt.(using fst (vbox (list ~sep:sp Utils.C_identifier.pp)))
    ;;

    let run_print
        (output_mode : [ `All | `Vars ])
        (vast : B.t) (oc : Out_channel.t)
      : Output.t Or_error.t =
      let cvars = B.cvars vast in
      let post  = B.postcondition vast in
      let f = Format.formatter_of_out_channel oc in
      Fmt.pf f "%a@." (pp output_mode)
        (Utils.C_identifier.Map.keys cvars, vast);
      Or_error.return { Output.cvars; post }
    ;;

    let run { Utils.Filter.aux; src; _ } ic oc : Output.t Or_error.t =
      let open Or_error.Let_syntax in
      let%bind ast =
        B.Frontend.load_from_ic ~path:(Utils.Io.In_source.to_string src) ic
      in
      let%bind vast = B.process ast in
      match aux with
      | Print output_mode -> run_print output_mode vast oc
      | Delitmus -> run_delitmus vast oc
      | Fuzz { seed; o } -> run_fuzz ~seed ~o vast oc
    ;;
  end)

module Normal_C : Utils.Filter.S with type aux_i = mode and type aux_o = Output.t =
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
      Var_scope.brand Var_scope.Unknown (Mini.Program.cvars prog)
    ;;

    let cvars_of_delitmus = Nothing.unreachable_code
    let postcondition = Fn.const None

    let delitmus (_ : t) : del Or_error.t =
      Or_error.error_string "Can't delitmus a normal C file"
    ;;

    let pp_del _ (x : del) : unit = Nothing.unreachable_code x
  end)
;;

module Litmus : Utils.Filter.S with type aux_i = mode and type aux_o = Output.t =
  Make (struct
    type ast = Ast.Litmus.t
    type t = Mini_litmus.Ast.Validated.t
    type del = Delitmus.Output.t

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

    let pp_del : del Fmt.t =
      Fmt.(
        prefix pp_prelude
          (using (Fn.compose Mini_reify.program Delitmus.Output.program)
             (vbox Ast.Translation_unit.pp)
          )
      )
    ;;

    let delitmus = Delitmus.run

    let fuzz : seed:int option -> o:Lib.Output.t -> t -> t Or_error.t = Fuzzer.run

    let postcondition
      : Mini_litmus.Ast.Validated.t -> Mini_litmus.Ast.Postcondition.t option =
      Mini_litmus.Ast.Validated.postcondition

    let cvars_of_delitmus (output : del) =
      let globals = Delitmus.Output.c_globals output in
      let locals  = Delitmus.Output.c_locals output in
      let map_opt = Var_scope.make_map_opt ~globals ~locals () in
      Option.value ~default:Utils.C_identifier.Map.empty map_opt
    ;;

    (* TODO(@MattWindsor91): split this into globals/locals. *)
    let cvars prog =
      Var_scope.brand (Var_scope.Unknown) (Mini_litmus.cvars prog)
    ;;
  end)
;;

let c_module (is_c : bool)
  : (module Utils.Filter.S with type aux_i = mode and type aux_o = Output.t) =
  if is_c then (module Normal_C) else (module Litmus)
;;
