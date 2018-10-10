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
open Lang

module type S = sig
  type statement

  type stm_explanation =
    { original : statement
    ; abs_type : Language.AbsStatement.t
    ; flags : Language.AbsStatement.FlagSet.t
    }

  type t =
    { statements : stm_explanation list
    }

  include Pretty_printer.S with type t := t

  val explain : statement list -> t
end

module Make (LS : Language.Intf) =
  struct
    type statement = LS.Statement.t

    type stm_explanation =
      { original : statement
      ; abs_type : Language.AbsStatement.t
      ; flags : Language.AbsStatement.FlagSet.t
      }

    type t =
      { statements : stm_explanation list
      }

    let explain_statement jsyms stm =
      { original = stm
      ; abs_type = LS.Statement.abs_type stm
      ; flags = LS.Statement.flags ~jsyms stm
      }

    let explain prog =
      let jsyms = LS.jump_symbols prog in
      { statements = List.map ~f:(explain_statement jsyms) prog
      }

    let stringify_ins =
      (* TODO(@MattWindsor91): merge with pp? *)
      let open Language.AbsInstruction in
      function
      | Arith   -> "arithmetic"
      | Call    -> "calling convention"
      | Compare -> "comparison"
      | Fence   -> "memory fence"
      | Jump    -> "jump"
      | Move    -> "move"
      | Nop     -> "no operation"
      | Return  -> "return to caller"
      | Stack   -> "stack"
      | Other   -> "other"
      | Unknown -> "?? instruction ??"

    let stringify_stm_basic =
      (* TODO(@MattWindsor91): merge with pp? *)
      let open Language.AbsStatement in
      function
      | Blank -> ""
      | Directive _ -> "directive"
      | Label _ -> "label"
      | Instruction ins -> stringify_ins ins
      | Other -> "??"

    let pp_statement f exp =
      (* TODO(@MattWindsor91): emit '<-- xyz' in a comment *)
      let open Language.AbsStatement in
      match exp.abs_type with
      | Blank -> () (* so as not to clutter up blank lines *)
      | _ ->
         Format.fprintf f "@[<h>%a@ <--@ @[%s%a@]@]@,"
                        LS.Statement.pp exp.original
                        (stringify_stm_basic exp.abs_type)
                        Language.AbsStatement.FlagSet.pp exp.flags

    let pp f exp =
      Format.pp_open_vbox f 0;
      List.iter ~f:(pp_statement f) exp.statements;
      Format.pp_close_box f ()
  end
