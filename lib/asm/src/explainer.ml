(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to permit
   persons to whom the Software is furnished to do so, subject to the
   following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
   NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
   DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
   OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
   USE OR OTHER DEALINGS IN THE SOFTWARE. *)

open Base
module Tx = Travesty_base_exts

module Config = struct
  module Format = struct
    type t = Assembly | Detailed [@@deriving sexp, equal]

    let default = Assembly
  end

  type t = {format: Format.t [@default Format.default]}
  [@@deriving sexp, equal, make]

  let default : t = make ()
end

module type S = Explainer_intf.S with type config := Config.t

module type S_filter = Runner_intf.S with type cfg = Config.t

let pp_set_adj pp_set f set =
  if not (Set.is_empty set) then Fmt.(prefix sp pp_set) f set

module Make (B : Explainer_intf.Basic) : S with module Lang = B.Src_lang =
struct
  module Lang = B.Src_lang

  type config = Config.t

  module Loc_explanation = Explanation.Make_loc (Lang.Location)

  module Ops_explanation = struct
    include Explanation.Make_ops (Lang.Instruction)

    include Act_abstract.Operand.Bundle.Inherit_properties
              (Act_abstract.Operand.Bundle)
              (struct
                type nonrec t = t

                let component = abstract
              end)
  end

  module Ins_explanation = struct
    include Explanation.Make_ins (struct
      module Location = Lang.Location
      module Instruction = Lang.Instruction
      module Loc_expl = Loc_explanation
      module Ops_expl = Ops_explanation
    end)

    include Act_abstract.Instruction.Inherit_properties
              (Act_abstract.Instruction)
              (struct
                type nonrec t = t

                let component = abstract
              end)
  end

  module Stm_explanation = struct
    include Explanation.Make_stm (struct
      module Instruction = Lang.Instruction
      module Ins_expl = Ins_explanation
      module Statement = Lang.Statement
    end)

    include Act_abstract.Statement.Inherit_properties
              (Act_abstract.Statement)
              (struct
                type nonrec t = t

                let component = abstract
              end)
  end

  type t =
    { statements: Stm_explanation.t list
    ; symbol_table: Act_abstract.Symbol.Table.t }

  let explain_statement syms stm =
    Stm_explanation.make ~context:syms ~original:stm

  let explain_program syms (prog : Lang.Program.t) =
    prog |> Lang.Program.listing |> List.map ~f:(explain_statement syms)

  let explain prog symbol_table =
    {statements= explain_program symbol_table prog; symbol_table}

  let pp_generic_statement_explanation f exp =
    Stm_explanation.(
      Fmt.pf f "@[<--@ @[%a%a@]@]" Act_abstract.Statement.Kind.pp
        (abs_kind exp) (pp_set_adj Flag.pp_set) (abs_flags exp))

  let pp_instruction_explanation f exp ins =
    Ins_explanation.(
      Fmt.pf f "@[<--@ @[%a%a%a@]@]" Act_abstract.Instruction.Kind.pp
        (abs_kind ins) (pp_set_adj Flag.pp_set) (abs_flags ins)
        (pp_set_adj Stm_explanation.Flag.pp_set)
        (Stm_explanation.abs_flags exp))

  let pp_explanation f exp =
    match Stm_explanation.(instructions (details exp)) with
    | [] ->
        pp_generic_statement_explanation f exp
    (* Assume at most one instruction per statement, for now *)
    | ins :: _ ->
        pp_instruction_explanation f exp ins

  let pp_statement f exp =
    (* TODO(@MattWindsor91): emit '<-- xyz' in a comment *)
    Stm_explanation.(
      match abs_kind exp with
      | Act_abstract.Statement.Kind.Blank ->
          () (* so as not to clutter up blank lines *)
      | _ ->
          Fmt.pf f "@[<h>%a@ %a@]" Lang.Statement.pp (original exp)
            (Lang.pp_comment ~pp:pp_explanation)
            exp)

  let non_blank_statements exp =
    Tx.List.exclude ~f:Stm_explanation.is_blank exp.statements

  let pp_as_assembly =
    Fmt.(vbox (using non_blank_statements (list pp_statement ~sep:sp)))

  let pp f exp =
    Fmt.(
      pf f "@[<v>%a@,@]"
        (list Stm_explanation.pp ~sep:sp)
        (non_blank_statements exp))

  let print_symbol_table ?(oc : Stdio.Out_channel.t option) (exp : t) : unit
      =
    Act_abstract.Symbol.Table.print_as_table ?oc exp.symbol_table

  let pp_for_explain_format : Config.Format.t -> t Fmt.t = function
    | Assembly ->
        pp_as_assembly
    | Detailed ->
        pp

  let index : 'a list -> (int, 'a) List.Assoc.t =
    List.mapi ~f:(fun i v -> (i, v))

  let output_explanation output_format name outp exps redirects =
    let f = Caml.Format.formatter_of_out_channel outp in
    let pp =
      Fmt.(
        using index
          (vbox
             (list ~sep:sp
                (pair ~sep:sp
                   (hbox (prefix (unit "-- program ") int))
                   (pp_for_explain_format output_format)))))
    in
    Fmt.pf f "%a@." pp exps ;
    Job.Output.make (Fmt.always "?") name redirects []

  module LS = B.Src_lang
  module San = Act_sanitiser.Instance.Make (B.Sanitiser_hook)

  let explain_san_program (program : San.Output.Program.t) : t =
    let listing = San.Output.Program.listing program in
    let s_table = San.Output.Program.symbol_table program in
    explain listing s_table

  let explain_san_programs : San.Output.Program.t list -> t list =
    List.map ~f:explain_san_program

  let run_explanation (outp : Stdio.Out_channel.t) ~(in_name : string)
      ~(program : LS.Program.t) ~(symbols : LS.Symbol.t list)
      ~(config : config) ~(passes : Set.M(Act_sanitiser.Pass_group).t) :
      Job.Output.t Or_error.t =
    let open Or_error.Let_syntax in
    let%map san = San.sanitise ~passes ~symbols program in
    let programs = San.Output.programs san in
    let explanations = explain_san_programs programs in
    let redirects = San.Output.redirects san in
    output_explanation config.format in_name outp explanations
      (LS.Symbol.R_map.to_string_alist redirects)

  module Filter : Runner_intf.S with type cfg = Config.t =
  Runner.Make (struct
    type cfg = Config.t

    module Symbol = B.Src_lang.Symbol
    module Program = B.Program

    let name = "Explainer"

    let tmp_file_ext = "txt"

    let default_config () = Config.default

    let run = run_explanation
  end)
end
