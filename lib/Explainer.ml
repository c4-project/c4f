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
open Utils

type stm_flag =
  [ `UnusedLabel
  | `ProgBoundary
  ] [@@deriving enum, sexp]

module FlagTable =
  StringTable.Make (
      struct
        type t = stm_flag

        let table =
          [ `UnusedLabel, "unused label"
          ; `ProgBoundary, "program boundary"
          ]
      end
    )

let pp_flag f flag =
  flag
  |> FlagTable.to_string
  |> Option.value ~default:"??"
  |> String.pp f

module FlagSet =
  struct
    include
      Set.Make(
          struct
            type t = stm_flag

            let compare x y =
              Int.compare (stm_flag_to_enum x)
                          (stm_flag_to_enum y)

            let sexp_of_t = sexp_of_stm_flag
            let t_of_sexp = stm_flag_of_sexp
          end
        )

    let pp f fset =
      match Set.to_list fset with
      | [] -> ()
      | xs ->
         Format.pp_print_space f ();
         Format.pp_print_char f '(';
         Format.pp_open_hovbox f 0;
         Format.pp_print_list ~pp_sep:(fun f _ -> Format.fprintf f ",@ ") pp_flag f xs;
         Format.pp_close_box f ();
         Format.pp_print_char f ')'
    end

module type S = sig
  type statement

  type stm_explanation =
    { original : statement
    ; abs_type : Language.abs_statement
    ; flags : FlagSet.t
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
      ; abs_type : Language.abs_statement
      ; flags : FlagSet.t
      }

    type t =
      { statements : stm_explanation list
      }

    let explain_statement jsyms stm =
      { original = stm
      ; abs_type = LS.Statement.statement_type stm
      ; flags =
          [ LS.Statement.is_label stm &&
              Language.SymSet.is_empty
                (Language.SymSet.inter jsyms (LS.Statement.symbol_set stm)),
            `UnusedLabel
          ; LS.Statement.is_program_boundary stm,
            `ProgBoundary
          ]
          |> List.map ~f:(Tuple2.uncurry Option.some_if)
          |> List.filter_opt
          |> FlagSet.of_list
      }

    let explain prog =
      let jsyms = LS.jump_symbols prog in
      { statements = List.map ~f:(explain_statement jsyms) prog
      }

    let stringify_ins =
      function
      | Language.AIArith -> "arithmetic"
      | Language.AICall -> "calling convention"
      | Language.AIFence -> "memory fence"
      | Language.AIJump -> "jump"
      | Language.AIMove -> "move"
      | Language.AINop -> "no operation"
      | Language.AIStack -> "stack manipulation"
      | Language.AIOther -> "?? instruction ??"

    let stringify_stm_basic =
      function
      | Language.ASBlank -> ""
      | Language.ASDirective _ -> "directive"
      | Language.ASLabel _ -> "label"
      | Language.ASInstruction ins -> stringify_ins ins
      | Language.ASOther -> "??"

    let pp_statement f exp =
      match exp.abs_type with
      | Language.ASBlank -> ()
      | _ ->
         Format.fprintf f "@[<h>%a@ <--@ @[%s%a@]@]@,"
                        LS.Statement.pp exp.original
                        (stringify_stm_basic exp.abs_type)
                        FlagSet.pp exp.flags

    let pp f exp =
      Format.pp_open_vbox f 0;
      List.iter ~f:(pp_statement f) exp.statements;
      Format.pp_close_box f ()
  end
