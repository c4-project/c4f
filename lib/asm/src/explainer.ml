(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

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

open Core_kernel
module Tx = Travesty_core_kernel_exts
include Explainer_intf

module Config = struct
  module Format = struct
    type t = Assembly | Detailed [@@deriving sexp, equal]

    let default = Assembly
  end

  type t = {format: Format.t [@default Format.default]}
  [@@deriving sexp, equal, make]

  let default : t = make ()
end

let pp_details pp_header pp_body =
  Fmt.(
    hvbox ~indent:4 (append (suffix sp (hvbox pp_header)) (braces pp_body)))

let pp_named_details name pp_body f t =
  Fmt.(pp_details (const string name) pp_body f ((), t))

let pp_optional_details name pp_item =
  Fmt.option (pp_named_details name pp_item)

let pp_listed_details name pp_item f = function
  | [] ->
      ()
  | ts ->
      Fmt.(pp_named_details name (list ~sep:comma pp_item)) f ts

let pp_set_adj pp_set f set =
  if not (Set.is_empty set) then Fmt.(prefix sp pp_set) f set

module Make_explanation (B : Basic_explanation) :
  Explanation
  with type elt := B.elt
   and type context := B.context
   and type details := B.details
   and module Abs := B.Abs
   and module Flag := B.Flag = struct
  type t =
    { original: B.elt
    ; abstract: B.Abs.t
    ; abs_kind: B.Abs.Kind.t
    ; abs_flags: B.Flag.Set.t
    ; details: B.details }
  [@@deriving fields]

  let has_abs_kind k x = B.Abs.Kind.equal k x.abs_kind

  let abs_kind_in ks x = B.Abs.Kind.Set.mem ks x.abs_kind

  let make ~context ~original =
    { original
    ; details= B.make_details original context
    ; abstract= B.abstract original
    ; abs_kind= B.abs_kind original
    ; abs_flags= B.abs_flags original context }

  let pp_body f t =
    Fmt.(
      Fields.Direct.iter t
        ~original:(fun _ _ _ -> ())
        ~abs_kind:(fun _ _ -> pf f "@[kind:@ %a;@]@ " B.Abs.Kind.pp)
        ~abstract:(fun _ _ _ -> ())
        ~abs_flags:(fun _ _ flags ->
          if not (B.Flag.Set.is_empty flags) then
            pf f "@[flags:@ %a;@]@ " B.Flag.pp_set flags )
        ~details:(fun _ _ -> B.pp_details f))

  let pp f t = pp_details B.pp pp_body f (original t, t)
end

module Make (B : Runner.Basic) :
  S with module Lang = B.Src_lang and type config = Config.t = struct
  module Lang = B.Src_lang

  type config = Config.t

  module Loc_explanation = struct
    module Flag = Act_abstract.Location.Flag

    module Base = struct
      module Abs = Act_abstract.Location
      module Flag = Flag

      type elt = Lang.Location.t

      let pp = Lang.Location.pp

      type context = Act_abstract.Symbol.Table.t

      type details = unit

      let make_details _ _ = ()

      let pp_details _f _context = ()

      include Act_abstract.Abstractable.Make (struct
        module Abs = Abs

        type t = elt

        let abstract = Lang.Location.abstract
      end)

      let abs_flags _ _ = Flag.Set.empty
    end

    type details = Base.details

    include Make_explanation (Base)
  end

  module Ops_explanation = struct
    module Flag = Act_abstract.Operand.Bundle.Flag

    module Base = struct
      module Abs = Act_abstract.Operand.Bundle
      module Flag = Flag

      type elt = Lang.Instruction.t

      let pp = Lang.Instruction.pp_operands

      type context = Act_abstract.Symbol.Table.t

      type details = unit

      let make_details _ _ = ()

      let pp_details _f _context = ()

      include Act_abstract.Abstractable.Make (struct
        module Abs = Abs

        type t = elt

        let abstract = Lang.Instruction.abs_operands
      end)

      let abs_flags ins =
        Act_abstract.Operand.Bundle.flags
          (Lang.Instruction.abs_operands ins)
    end

    type details = Base.details

    include Make_explanation (Base)

    include Act_abstract.Operand.Bundle.Inherit_properties
              (Act_abstract.Operand.Bundle)
              (struct
                type nonrec t = t

                let component = abstract
              end)
  end

  module Ins_explanation = struct
    module Flag = Act_abstract.Instruction.Flag

    module Base = struct
      module Abs = Act_abstract.Instruction
      module Flag = Flag

      type elt = Lang.Instruction.t

      let pp = Lang.Instruction.pp

      type context = Act_abstract.Symbol.Table.t

      type details =
        { operands: Ops_explanation.t option
        ; locations: Loc_explanation.t list }
      [@@deriving fields]

      let make_operand_details ins context =
        if Lang.Instruction.On_operands.is_none ins then None
        else Some (Ops_explanation.make ~context ~original:ins)

      let make_location_details ins context =
        let locations = Lang.Instruction.On_locations.to_list ins in
        List.map locations ~f:(fun original ->
            Loc_explanation.make ~context ~original )

      let make_details ins context =
        { operands= make_operand_details ins context
        ; locations= make_location_details ins context }

      let pp_operand_details =
        pp_optional_details "operands" Ops_explanation.pp

      let pp_location_details =
        pp_listed_details "location" Loc_explanation.pp

      let pp_details f {operands; locations} =
        Fmt.pf f "%a@ %a" pp_operand_details operands pp_location_details
          locations

      include (
        Lang.Instruction :
          Act_abstract.Abstractable.S
          with module Abs := Abs
           and type t := elt )

      let abs_flags _ _ = Flag.Set.empty
    end

    type details = Base.details

    include Make_explanation (Base)

    include Act_abstract.Instruction.Inherit_properties
              (Act_abstract.Instruction)
              (struct
                type nonrec t = t

                let component = abstract
              end)
  end

  module Stm_explanation = struct
    module Flag = Lang.Statement.Extended_flag

    module Base = struct
      module Abs = Act_abstract.Statement
      module Flag = Flag

      type elt = Lang.Statement.t

      let pp = Lang.Statement.pp

      type context = Act_abstract.Symbol.Table.t

      type details = {instructions: Ins_explanation.t list}
      [@@deriving fields]

      let describe_instructions stm context =
        match Lang.Statement.abs_kind stm with
        | Act_abstract.Statement.Kind.Instruction ->
            stm |> Lang.Statement.On_instructions.to_list
            |> List.map ~f:(fun original ->
                   Ins_explanation.make ~original ~context )
        | Blank | Unknown | Directive | Label ->
            []

      let make_details stm context =
        {instructions= describe_instructions stm context}

      let pp_instruction_details =
        pp_listed_details "instruction" Ins_explanation.pp

      let pp_details f =
        Fields_of_details.Direct.iter ~instructions:(fun _ _ ->
            pp_instruction_details f )

      include (
        Lang.Statement :
          Act_abstract.Abstractable.S
          with module Abs := Abs
           and type t := elt )

      let abs_flags = Lang.Statement.extended_flags
    end

    type details = Base.details

    include Make_explanation (Base)

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
    match (Stm_explanation.details exp).instructions with
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

  let output_explanation output_format name outp exp redirects =
    let f = Format.formatter_of_out_channel outp in
    pp_for_explain_format output_format f exp ;
    Format.pp_print_newline f () ;
    Job.Output.make (Fmt.always "?") name redirects []

  module LS = B.Src_lang
  module SS = Act_sanitiser.Instance.Make_single (B.Sanitiser_hook)

  let run_explanation (_osrc : Act_utils.Io.Out_sink.t)
      (outp : Out_channel.t) ~(in_name : string) ~(program : LS.Program.t)
      ~(symbols : LS.Symbol.t list) ~(config : config)
      ~(passes : Act_config.Sanitiser_pass.Set.t) : Job.Output.t Or_error.t
      =
    let open Or_error.Let_syntax in
    let%map san = SS.sanitise ~passes ~symbols program in
    let program = SS.Output.programs san in
    let listing = SS.Output.Program.listing program in
    let s_table = SS.Output.Program.symbol_table program in
    let exp = explain listing s_table in
    let redirects = SS.Output.redirects san in
    output_explanation config.format in_name outp exp
      (LS.Symbol.R_map.to_string_alist redirects)

  module Filter : Runner.S with type cfg = Config.t = Runner.Make (struct
    type cfg = Config.t

    module Symbol = B.Src_lang.Symbol

    type program = B.Src_lang.Program.t

    let parse_asm _iname isrc _inp =
      Or_error.(B.Frontend.load_from_isrc isrc >>| B.program)

    let name = "Explainer"

    let tmp_file_ext = "txt"

    let default_config () = Config.default

    let run = run_explanation
  end)
end

let get_filter (module B : Runner.Basic) :
    (module Runner.S with type cfg = Config.t) =
  let module Exp = Make (B) in
  (module Exp.Filter)
