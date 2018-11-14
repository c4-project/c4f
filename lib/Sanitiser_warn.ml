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

module Make_null_hook (Lang : Language.S)
  : Hook with module Lang = Lang = struct
  module Lang = Lang

  (* No warnings possible *)
  type t
  let pp _ _ = ()
end

module Make (H : Hook)
  : S with module Hook = H = struct
  module Hook = H

  type elt =
    | Instruction of H.Lang.Instruction.t
    | Statement of H.Lang.Statement.t
    | Location of H.Lang.Location.t
    | Operands of H.Lang.Instruction.t
  ;;

  type body =
    | MissingEndLabel
    | UnknownElt of elt
    | ErroneousElt of elt * Error.t
    | SymbolRedirFail of H.Lang.Symbol.t
    | Custom of H.t
  ;;

  type t =
    { body     : body
    ; progname : string
    }
  ;;

  let pp_elt f = function
    | Statement s -> H.Lang.Statement.pp f s
    | Instruction i | Operands i -> H.Lang.Instruction.pp f i
    | Location l -> H.Lang.Location.pp f l
  ;;

  let elt_type_name = function
    | Statement _ -> "statement"
    | Instruction _ -> "instruction"
    | Operands _ -> "the operands of instruction"
    | Location _ -> "location"
  ;;

  let pp_unknown_warning f elt =
    Format.fprintf f
      "act didn't understand@ %s@ %a.@ The litmus translation may be wrong."
      (elt_type_name elt)
      pp_elt elt

  let pp_erroneous_warning f elt error =
    Format.fprintf f
      "act thinks@ %s@ %a@ is@ erroneous.@ The litmus translation may be wrong.@ Reasons:@ %a"
      (elt_type_name elt)
      pp_elt elt
      Sexp.pp_hum [%sexp (error : Error.t)]
  ;;

  let pp_symbol_redir_warning f src =
    Format.fprintf f
      "act couldn't find the symbol@ '%a'@ in the assembly.@ The litmus translation may have an incorrect location table."
      H.Lang.Symbol.pp src
  ;;

  let pp_body f =
    function
    | MissingEndLabel ->
      String.pp f
        "act needed an end-of-program label here, but there wasn't one."
    | SymbolRedirFail src -> pp_symbol_redir_warning f src
    | UnknownElt elt -> pp_unknown_warning f elt
    | ErroneousElt (elt, error) -> pp_erroneous_warning f elt error
    | Custom c -> H.pp f c

  let pp f ent =
    Format.fprintf f "In program %s:@ " ent.progname;
    pp_body f ent.body
end
