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
    | SymbolRedirFail of L.Symbol.t
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

  let pp_symbol_redir_warning f src =
    Format.fprintf f
      "act couldn't find the symbol@ '%a'@ in the assembly.@ The litmus translation may have an incorrect location table."
      L.Symbol.pp src
  ;;

  let pp_body f =
    function
    | MissingEndLabel ->
      String.pp f
        "act needed an end-of-program label here, but there wasn't one."
    | SymbolRedirFail src -> pp_symbol_redir_warning f src
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
    { progname  : string
    ; proglen   : int
    ; endlabel  : string option
    ; syms      : Abstract.Symbol.Table.t
    ; redirects : L.Symbol.R_map.t
    ; passes    : Pass.Set.t
    ; warnings  : Warn.t list
    }[@@deriving fields]

  let initial ~passes =
    { progname  = "(no program)"
    ; proglen   = 0
    ; endlabel  = None
    ; syms      = Abstract.Symbol.Table.empty
    ; redirects = L.Symbol.R_map.make L.Symbol.Set.empty
    ; passes
    ; warnings  = []
    }

  include State.Make_transform (struct
      type t = ctx
      module Inner = Or_error
    end)

  let is_pass_enabled pass =
    peek (fun ctx -> Pass.Set.mem ctx.passes pass)
  ;;

  let enter_program ~name prog =
    modify (
      fun ctx ->
        { ctx with progname = name
                 ; proglen = List.length prog
                 ; endlabel = None
                 ; syms = Abstract.Symbol.Table.empty }
    ) >>| fun () -> prog
  ;;

  let get_end_label = peek endlabel
  let set_end_label lbl =
    Monadic.modify (
      fun ctx ->
        match ctx.endlabel with
        | Some s ->
          Or_error.error_s
            [%message "Tried to overwrite end label"
                ~original:s
                ~replaced:lbl]
        | None ->
          Or_error.return
            { ctx with proglen = succ ctx.proglen;
                       endlabel = Some lbl }
    )
  ;;

  let get_prog_name = peek progname

  let get_prog_length = peek proglen
  let dec_prog_length =
    Monadic.modify (
      fun ctx ->
        if ctx.proglen <= 0
        then Or_error.error_s
            [%message "Underflow in program length"
                ~proglen:(ctx.proglen : int)]
        else Or_error.return { ctx with proglen = pred ctx.proglen }
    )
  ;;

  let (|->) pass f a =
    let open Let_syntax in
    if%bind is_pass_enabled pass then f a else return a
  ;;

  let warn w =
    modify (
      fun ctx ->
        let ent = { Warn.progname = ctx.progname; body = w } in
        { ctx with warnings = ent::ctx.warnings }
    )
  ;;

  let take_warnings =
    make (
      fun ctx ->
        let warnings = ctx.warnings in
        ( { ctx with warnings = [] }, warnings )
    )
  ;;

  let add_symbol sym sort =
    make (
      fun ctx ->
        { ctx with syms = Abstract.Symbol.Table.add ctx.syms sym sort }
      , sym
    )
  ;;

  let get_symbol_table = peek syms

  let get_symbols_with_sorts sorts =
    Abstract.Symbol.(
      let open Let_syntax in
      let%map all_syms = get_symbol_table in
      Table.set_of_sorts all_syms (Sort.Set.of_list sorts)
    )
  ;;

  let set_symbol_table syms =
    modify (fun ctx -> { ctx with syms = syms })
  ;;

  let resolve_redirect sym = function
    | L.Symbol.R_map.Identity -> sym
    | MapsTo sym' -> sym'
  ;;

  let get_redirect sym =
    let open Let_syntax in
    let%map rds = peek redirects in
    Option.map (L.Symbol.R_map.dest_of rds sym)
      ~f:(resolve_redirect sym)
  ;;

  let get_redirect_alist syms
    : ((L.Symbol.t, L.Symbol.t) List.Assoc.t) t =
    let open Let_syntax in
    let%map rds = peek redirects in
    List.filter_map syms
      ~f:(fun sym ->
          Option.(
            L.Symbol.R_map.dest_of rds sym
            >>| resolve_redirect sym
            >>| Tuple2.create sym
          )
        )
    ;;

  let redirect ~src ~dst =
    let open Let_syntax in
    let%bind rds = peek redirects in
    (* Redirection can fail, which _should_ be an internal bug, so
       we make it a fatal error in the sanitiser. *)
    Monadic.modify
      (fun ctx ->
         let open Or_error.Let_syntax in
         let%map rds' = L.Symbol.R_map.redirect ~src ~dst rds in
         { ctx with redirects = rds' })
  ;;

  let make_fresh_label prefix =
    Abstract.Symbol.(
      let open Let_syntax in
      let%bind syms = get_symbols_with_sorts [ Sort.Jump; Sort.Label ] in
      let l = freshen_label syms prefix in
      add_symbol l Sort.Label
    )
  ;;

  let make_fresh_heap_loc prefix =
    Abstract.Symbol.(
      let open Let_syntax in
      let%bind syms = get_symbols_with_sorts [ Sort.Heap ] in
      let l = freshen_label syms prefix in
      add_symbol l Sort.Heap
    )
  ;;
end
