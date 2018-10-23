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

module type S = sig
  type statement

  type stm_explanation =
    { original : statement
    ; abs_type : Abstract.Statement.t
    ; flags : Abstract.Statement.Flag.Set.t
    }

  type t =
    { statements : stm_explanation list
    }

  include Pretty_printer.S with type t := t

  val explain : statement list -> t
end

module Make (LS : Language.Intf) = struct
  type statement = LS.Statement.t

  type stm_explanation =
    { original : statement
    ; abs_type : Abstract.Statement.t
    ; flags : Abstract.Statement.Flag.Set.t
    }

  type t =
    { statements : stm_explanation list
    }

  let explain_statement syms stm =
    { original = stm
    ; abs_type = LS.Statement.abs_type stm
    ; flags = LS.Statement.flags ~syms stm
    }

  let explain prog =
    let syms = LS.symbols prog in
    { statements = List.map ~f:(explain_statement syms) prog
    }

  let stringify_stm_basic =
    (* TODO(@MattWindsor91): merge with pp? *)
    let open Abstract.Statement in
    function
    | Blank -> ""
    | Directive _ -> "directive"
    | Label _ -> "label"
    | Instruction ins -> Abstract.Instruction.to_string ins
    | Other -> "??"

  let pp_explanation f exp =
    Format.fprintf f "@[<--@ @[%s%a@]@]"
      (stringify_stm_basic exp.abs_type)
      Abstract.Statement.Flag.pp_set exp.flags

  let pp_statement f exp =
    (* TODO(@MattWindsor91): emit '<-- xyz' in a comment *)
    let open Abstract.Statement in
    match exp.abs_type with
    | Blank -> () (* so as not to clutter up blank lines *)
    | _ ->
      Format.fprintf f "@[<h>%a@ %a@]@,"
        LS.Statement.pp exp.original
        (LS.pp_comment ~pp:pp_explanation) exp

  let pp f exp =
    Format.pp_open_vbox f 0;
    List.iter ~f:(pp_statement f) exp.statements;
    Format.pp_close_box f ()
end
