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

module type Basic_explanation = sig
  type elt
  type context
  include Abstractable.S with type t := elt
  include Pretty_printer.S with type t := elt
  module Flag : Abstract_flag.S
  val abs_flags : elt -> context -> Flag.Set.t
end

module type Explanation = sig
  type t
  type elt
  type context
  include Abstractable.S with type t := t
  include Pretty_printer.S with type t := t
  module Flag : Abstract_flag.S
  val original : t -> elt
  val abs_flags : t -> Flag.Set.t
  val create : context:context -> original:elt -> t
end

module Make_explanation (B : Basic_explanation)
  : Explanation with type elt := B.elt
                 and type context := B.context
                 and module Abs := B.Abs
                 and module Flag := B.Flag = struct
  type t =
    { original  : B.elt
    ; abs_type  : B.Abs.t
    ; abs_flags : B.Flag.Set.t
    }
  [@@deriving fields]
  ;;

  let create ~context ~original =
    { original
    ; abs_type  = B.abs_type original
    ; abs_flags = B.abs_flags original context
    }
  ;;

  let pp f t =
    Format.pp_open_vbox f 0;
    Fields.Direct.iter t
      ~original:(fun _ _ ->
          Format.fprintf f "@[%a@]@ " B.pp)
      ~abs_type:(fun _ _ ->
          Format.fprintf f "@[^--@ type:@ %a@]@ " B.Abs.pp)
      ~abs_flags:(fun _ _ ->
          Format.fprintf f "@[ '-@ flags:@ %a@]" B.Flag.pp_set);
    Format.pp_close_box f ()
  ;;
end

module type S = sig
  type statement

  module Stm_explanation
    : Explanation with type elt := statement
                   and type context := Abstract.Symbol.Table.t
                   and module Abs := Abstract.Statement
  ;;

  type t =
    { statements : Stm_explanation.t list
    }

  include Pretty_printer.S with type t := t
  val pp_as_assembly : Base.Formatter.t -> t -> unit

  val explain : statement list -> t
end

module Make (LS : Language.S) : S with type statement := LS.Statement.t = struct

  module Ins_explanation = struct
    module Flag = Abstract.Instruction.Flag
    module Base = Make_explanation (struct
        module Abs = Abstract.Instruction
        module Flag = Flag

        type elt = LS.Instruction.t
        let pp = LS.Instruction.pp
        type context = unit

        let abs_type = LS.Instruction.abs_type
        let abs_flags = fun _ () -> Flag.Set.empty
      end)
    ;;

    include Base
  end

  module Stm_explanation = struct
    module Flag = LS.Statement.Flag
    module Base = Make_explanation (struct
        module Abs = Abstract.Statement
        module Flag = Flag

        type elt = LS.Statement.t
        let pp = LS.Statement.pp
        type context = Abstract.Symbol.Table.t

        let abs_type = LS.Statement.abs_type

        let abs_flags = LS.Statement.flags
      end)
    ;;

    include Base
  end

  type t =
    { statements : Stm_explanation.t list
    }

  let explain_statement syms stm =
    Stm_explanation.create ~context:syms ~original:stm
  ;;

  let explain prog =
    let syms = LS.symbols prog in
    { statements = List.map ~f:(explain_statement syms) prog
    }

  (* TODO(@MattWindsor91): merge with pp? *)
  let stringify_stm_basic = function
    | Abstract.Statement.Blank -> ""
    | Directive _ -> "directive"
    | Label _ -> "label"
    | Instruction ins -> Abstract.Instruction.(to_string (opcode ins))
    | Other -> "??"

  let pp_explanation f exp =
    Stm_explanation.(
      Format.fprintf f "@[<--@ @[%s%a@]@]"
        (stringify_stm_basic (abs_type exp))
        Flag.pp_set (abs_flags exp)
    )
  ;;

  let pp_statement f exp =
    (* TODO(@MattWindsor91): emit '<-- xyz' in a comment *)
    Stm_explanation.(
      match Stm_explanation.abs_type exp with
      | Abstract.Statement.Blank -> () (* so as not to clutter up blank lines *)
      | _ ->
        Format.fprintf f "@[<h>%a@ %a@]"
          LS.Statement.pp (original exp)
          (LS.pp_comment ~pp:pp_explanation) exp
    )
  ;;

  let pp_as_assembly f exp =
    Format.fprintf f "@[<v>%a@]"
      (Format.pp_print_list pp_statement ~pp_sep:Format.pp_print_space)
      exp.statements
  ;;

  let pp f exp =
    Format.fprintf f "@[<v>%a@]"
      (Format.pp_print_list Stm_explanation.pp ~pp_sep:Format.pp_print_space)
      exp.statements
  ;;
end
