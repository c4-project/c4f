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

include Sanitiser_warn_intf

module Make (Lang : Language.S) : S with module Lang := Lang = struct
  type elt =
    | Instruction of Lang.Instruction.t
    | Location of Lang.Location.t
    | Operands of Lang.Instruction.t
    | Statement of Lang.Statement.t
    | Symbol of Lang.Symbol.t
  ;;

  type t =
    { program_name : string
    ; element      : elt
    ; body         : Info.t
    }
    [@@deriving fields, make]
  ;;

  let pp_elt f = function
    | Statement s -> Lang.Statement.pp f s
    | Instruction i | Operands i -> Lang.Instruction.pp f i
    | Location l -> Lang.Location.pp f l
    | Symbol s -> Lang.Symbol.pp f s
  ;;

  let elt_type_name = function
    | Instruction _ -> "instruction"
    | Location _ -> "location"
    | Operands _ -> "operands of instruction"
    | Statement _ -> "statement"
    | Symbol _ -> "symbol"
  ;;

  let pp f ent =
    Format.fprintf f "@[<v>@[<h>In program %s,@ in %s@ `%a`:@]@ %a@]"
      ent.program_name
      (elt_type_name ent.element)
      pp_elt ent.element
      Info.pp ent.body
  ;;

  let not_understood () =
    Info.of_string "act didn't understand this element"
  ;;

  let erroneous reason =
    Info.create_s
      [%message
        "act thinks this element is invalid"
          ~reason:(reason : Error.t)
      ]
  ;;

end
