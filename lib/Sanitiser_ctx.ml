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

open Core
open Utils

include Sanitiser_ctx_intf

(*
 * Warnings
 *)

module NoCustomWarn : CustomWarnS = struct
  (* No warnings possible *)
  type t
  let pp _ _ = ()
end

module Warn (L : Language.Intf) (C : CustomWarnS)
  : WarnIntf with module L = L and module C = C = struct
  module L = L
  module C = C

  type elt =
    | Instruction of L.Instruction.t
    | Statement of L.Statement.t
    | Location of L.Location.t
    | Operands of L.Instruction.t

  type body =
    | MissingEndLabel
    | UnknownElt of elt
    | ErroneousElt of elt
    | Custom of C.t

  type t =
    { body     : body
    ; progname : string
    }

  let pp_elt f =
    function
    | Statement s -> L.Statement.pp f s
    | Instruction i | Operands i -> L.Instruction.pp f i
    | Location l -> L.Location.pp f l
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

  let pp_erroneous_warning f elt =
    Format.fprintf f
      "act thinks@ %s@ %a@ is@ erroneous.@ The litmus translation may be wrong."
      (elt_type_name elt)
      pp_elt elt

  let pp_body f =
    function
    | MissingEndLabel ->
      String.pp f
        "act needed an end-of-program label here, but there wasn't one."
    | UnknownElt elt -> pp_unknown_warning f elt
    | ErroneousElt elt -> pp_erroneous_warning f elt
    | Custom c -> C.pp f c

  let pp f ent =
    Format.fprintf f "In program %s:@ " ent.progname;
    pp_body f ent.body
end


(*
 * Context
 *)

let freshen_label (syms : Abstract.Symbol.Set.t) (prefix : string) : string =
  let rec mu prefix count =
    let str = sprintf "%s%d" prefix count in
    if Abstract.Symbol.Set.mem syms str
    then mu prefix (count + 1)
    else str
  in
  mu prefix 0
;;

module Make (L : Language.Intf) (C : CustomWarnS)
 : Intf with module Lang = L and module Warn.C = C = struct
  module Lang = L
  module Warn = Warn (L) (C)
  module Pass = Sanitiser_pass

  type ctx =
    { progname : string
    ; proglen  : int
    ; endlabel : string option
    ; syms     : Abstract.Symbol.Table.t
    ; passes   : Pass.Set.t
    ; warnings : Warn.t list
    }[@@deriving fields]

  let initial ~passes ~progname ~proglen =
    { progname
    ; proglen
    ; endlabel = None
    ; syms     = Abstract.Symbol.Table.empty
    ; passes
    ; warnings = []
    }

  include State.Make (struct type state = ctx end)

  let is_pass_enabled pass =
    peek (fun ctx -> Pass.Set.mem ctx.passes pass)
  ;;

  let end_label = peek (fun ctx -> ctx.endlabel)

  let (|->) pass f a =
    make
      (fun ctx ->
         if Pass.Set.mem ctx.passes pass
         then run (f a) ctx
         else (ctx, a))
  ;;

  let warn w =
    modify
      ( fun ctx ->
         let ent = { Warn.progname = ctx.progname; body = w } in
         { ctx with warnings = ent::ctx.warnings }
      )
      ()
  ;;

  let add_sym sym sort =
    make
      (fun ctx ->
         { ctx with syms = Abstract.Symbol.Table.add ctx.syms sym sort }
         , sym
      )
  ;;

  let syms_with_sorts sorts =
    Abstract.Symbol.(
      let open Let_syntax in
      let%bind all_syms = peek (Field.get Fields_of_ctx.syms) in
      return (Table.set_of_sorts all_syms (Sort.Set.of_list sorts))
    )
  ;;

  let make_fresh_label prefix =
    Abstract.Symbol.(
      let open Let_syntax in
      let%bind syms = syms_with_sorts [ Sort.Jump; Sort.Label ] in
      let l = freshen_label syms prefix in
      add_sym l Sort.Label
    )
  ;;

  let make_fresh_heap_loc prefix =
    Abstract.Symbol.(
      let open Let_syntax in
      let%bind syms = syms_with_sorts [ Sort.Heap ] in
      let l = freshen_label syms prefix in
      add_sym l Sort.Heap
    )
  ;;
end
