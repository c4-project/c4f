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

open Core_kernel
open Utils

module type Basic_explanation = sig
  type elt
  type context
  type details
  include Abstractable.S with type t := elt
  include Pretty_printer.S with type t := elt
  module Flag : Abstract_flag.S
  val abs_flags : elt -> context -> Flag.Set.t
  val make_details : elt -> context -> details
  val pp_details : Format.formatter -> details -> unit
end

module type Explanation = sig
  type t
  type elt
  type details
  type context
  include Abstractable.S with type t := t
  include Pretty_printer.S with type t := t
  module Flag : Abstract_flag.S
  val original : t -> elt
  val details : t -> details
  val abs_flags : t -> Flag.Set.t
  val make : context:context -> original:elt -> t
end

module Make_explanation (B : Basic_explanation)
  : Explanation with type elt := B.elt
                 and type context := B.context
                 and type details := B.details
                 and module Abs := B.Abs
                 and module Flag := B.Flag = struct
  type t =
    { original  : B.elt
    ; abstract  : B.Abs.t
    ; abs_kind  : B.Abs.Kind.t
    ; abs_flags : B.Flag.Set.t
    ; details   : B.details
    }
  [@@deriving fields]
  ;;

  let has_abs_kind k x = B.Abs.Kind.equal k x.abs_kind
  let abs_kind_in ks x = B.Abs.Kind.Set.mem ks x.abs_kind

  let make ~context ~original =
    { original
    ; details   = B.make_details original context
    ; abstract  = B.abstract original
    ; abs_kind  = B.abs_kind original
    ; abs_flags = B.abs_flags original context
    }
  ;;

  let pp f t =
    Format.pp_open_vbox f 4;
    Fields.Direct.iter t
      ~original:(fun _ _ ->
          Format.fprintf f "@[[@,%a]@,@]" B.pp)
      ~abs_kind:(fun _ _ ->
          Format.fprintf f "@ @[kind:@ %a@]" B.Abs.Kind.pp)
      ~abstract:(fun _ _ _ -> ())
      ~abs_flags:(fun _ _ flags ->
          if not (B.Flag.Set.is_empty flags)
          then Format.fprintf f "@ @[flags:@ %a@]" B.Flag.pp_set flags)
      ~details:(fun _ _ -> B.pp_details f);
    Format.pp_close_box f ()
  ;;
end

module type S = sig
  module Lang : Language.S

  module Ops_explanation : sig
    include Explanation with type elt := Lang.Instruction.t
                         and type context := Abstract.Symbol.Table.t
                         and module Abs := Abstract.Operand.Bundle
    ;;

    include Abstract.Operand.Bundle.S_properties with type t := t
  end

  module Ins_explanation : sig
    include Explanation with type elt := Lang.Instruction.t
                         and type context := Abstract.Symbol.Table.t
                         and module Abs := Abstract.Instruction
    ;;

    include Abstract.Instruction.S_properties with type t := t
  end

  module Stm_explanation : sig
    include Explanation with type elt := Lang.Statement.t
                         and type context := Abstract.Symbol.Table.t
                         and module Abs := Abstract.Statement
    ;;

    include Abstract.Statement.S_properties with type t := t
  end

  type t =
    { statements : Stm_explanation.t list
    }

  include Pretty_printer.S with type t := t
  val pp_as_assembly : Base.Formatter.t -> t -> unit

  val explain : Lang.Statement.t list -> t
end

module Make (Lang : Language.S) : S with module Lang := Lang = struct
  module Ops_explanation = struct
    module Flag = Abstract.Operand.Bundle.Flag
    module Base = struct
      module Abs = Abstract.Operand.Bundle
      module Flag = Flag

      type elt = Lang.Instruction.t
      let pp = Lang.Instruction.pp_operands
      type context = Abstract.Symbol.Table.t

      type details = unit

      let make_details _ _ = ()
      let pp_details _f _context = ()

      include Abstractable.Make (struct
          module Abs = Abs
          type t = elt
          let abstract = Lang.Instruction.abs_operands
        end)

      let abs_flags ins =
        Abstract.Operand.Bundle.flags
          (Lang.Instruction.abs_operands ins)
    end

    type details = Base.details

    include Make_explanation (Base)
    include Abstract.Operand.Bundle.Inherit_properties
        (Abstract.Operand.Bundle)
        (struct
          type nonrec t = t
          let component = abstract
        end)
  end

  module Ins_explanation = struct
    module Flag = Abstract.Instruction.Flag
    module Base = struct
      module Abs = Abstract.Instruction
      module Flag = Flag

      type elt = Lang.Instruction.t
      let pp = Lang.Instruction.pp
      type context = Abstract.Symbol.Table.t

      type details =
        { operands : Ops_explanation.t option }
      [@@deriving fields]
      ;;

      let make_details ins context =
        { operands =
            if Lang.Instruction.On_operands.is_none ins
            then None
            else Some (Ops_explanation.make ~context ~original:ins)
        }
      ;;

      let pp_details f { operands } =
        My_format.pp_option f
          ~pp:(fun f -> Format.fprintf f "@,@[<v 4>Operand details:@,%a@]"
                 Ops_explanation.pp)
          operands
      ;;

      include
        ( Lang.Instruction : Abstractable.S
          with module Abs := Abs and type t := elt
        )
      ;;
      let abs_flags = fun _ _ -> Flag.Set.empty
    end

    type details = Base.details

    include Make_explanation (Base)
    include Abstract.Instruction.Inherit_properties
        (Abstract.Instruction)
        (struct
          type nonrec t = t
          let component = abstract
        end)
  end

  module Stm_explanation = struct
    module Flag = Lang.Statement.Extended_flag
    module Base = struct
      module Abs = Abstract.Statement
      module Flag = Flag

      type elt = Lang.Statement.t
      let pp = Lang.Statement.pp
      type context = Abstract.Symbol.Table.t

      type details =
        { instructions : Ins_explanation.t list }
      [@@deriving fields]
      ;;

      let describe_instructions stm context =
        match Lang.Statement.abs_kind stm with
        | Abstract.Statement.Kind.Instruction ->
          stm
          |> Lang.Statement.On_instructions.to_list
          |> List.map
            ~f:(fun original ->
                Ins_explanation.make ~original ~context)
        | Blank | Unknown | Directive | Label -> []
      ;;

      let make_details stm context =
        { instructions =
            describe_instructions stm context }
      ;;

      let pp_instruction_details f = function
        | [] -> ()
        | ins ->
          Format.fprintf f "@,@[<v 4>Instruction details:@,%a@]"
            (Format.pp_print_list ~pp_sep:Format.pp_print_space
               Ins_explanation.pp) ins
      ;;

      let pp_details f =
        Fields_of_details.Direct.iter
          ~instructions:(fun _ _ -> pp_instruction_details f)
      ;;

      include
        ( Lang.Statement : Abstractable.S
          with module Abs := Abs and type t := elt
        )
      ;;
      let abs_flags = Lang.Statement.extended_flags
    end

    type details = Base.details

    include Make_explanation (Base)
    include Abstract.Statement.Inherit_properties
        (Abstract.Statement)
        (struct
          type nonrec t = t
          let component = abstract
        end)
  end

  type t =
    { statements : Stm_explanation.t list
    }

  let explain_statement syms stm =
    Stm_explanation.make ~context:syms ~original:stm
  ;;

  let explain prog =
    let syms = Lang.symbols prog in
    { statements = List.map ~f:(explain_statement syms) prog
    }

  let pp_generic_statement_explanation f exp =
    Stm_explanation.(
      Format.fprintf f "@[<--@ @[%a%a@]@]"
        Abstract.Statement.Kind.pp (abs_kind exp)
        Flag.pp_set (abs_flags exp)
    )
  ;;

  let pp_instruction_explanation f exp ins =
    Ins_explanation.(
      Format.fprintf f "@[<--@ @[%a%a%a@]@]"
        Abstract.Instruction.Kind.pp (abs_kind ins)
        Flag.pp_set (abs_flags ins)
        Stm_explanation.Flag.pp_set (Stm_explanation.abs_flags exp)
    )
  ;;

  let pp_explanation f exp =
    match (Stm_explanation.details exp).instructions with
    | [] -> pp_generic_statement_explanation f exp
    (* Assume at most one instruction per statement, for now *)
    | ins :: _ -> pp_instruction_explanation f exp ins
  ;;

  let pp_statement f exp =
    (* TODO(@MattWindsor91): emit '<-- xyz' in a comment *)
    Stm_explanation.(
      match abs_kind exp with
      | Abstract.Statement.Kind.Blank -> () (* so as not to clutter up blank lines *)
      | _ ->
        Format.fprintf f "@[<h>%a@ %a@]"
          Lang.Statement.pp (original exp)
          (Lang.pp_comment ~pp:pp_explanation) exp
    )
  ;;

  let non_blank_statements exp =
    My_list.exclude ~f:Stm_explanation.is_blank exp.statements
  ;;

  let pp_as_assembly f exp =
    Format.fprintf f "@[<v>%a@]"
      (Format.pp_print_list pp_statement ~pp_sep:Format.pp_print_space)
      (non_blank_statements exp)
  ;;

  let pp f exp =
    Format.fprintf f "@[<v>%a@]"
      (Format.pp_print_list Stm_explanation.pp ~pp_sep:Format.pp_print_space)
      (non_blank_statements exp)
  ;;
end
