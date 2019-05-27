(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

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

open Base

module Make (B : Pass_intf.Basic) :
  Pass_intf.S
  with type t := B.Lang.Program.t list
   and type 'a ctx := 'a B.Ctx.t = struct
  module Ctx_Pcon = Travesty_base_exts.List.On_monad (B.Ctx)
  module Ctx_Prog_Sym = B.Lang.Program.On_symbols.On_monad (B.Ctx)

  let over_all_symbols progs ~f =
    (* Nested mapping: over symbols in statements in statement lists in
       programs. *)
    Ctx_Pcon.map_m progs ~f:(Ctx_Prog_Sym.map_m ~f)

  let get_existing_redirect_or sym ~f =
    B.Ctx.Let_syntax.(
      let%bind sym' = B.Ctx.get_redirect sym in
      if B.Lang.Symbol.equal sym sym' then f sym else B.Ctx.return sym')

  module Escape = Act_language.Symbol_escape.Make (B.Lang.Symbol)

  let add_escape_redirects : unit B.Ctx.t =
    B.Ctx.Let_syntax.(
      let%bind to_escape = B.Ctx.get_variables in
      B.Ctx.modify_rmap
        ~f:(Fn.compose Or_error.return (Escape.escape_rmap ~to_escape)))

  (** [redirect_or_escape sym] gets the redirected form of [sym], if one
      exists, or the escaped form of [sym] otherwise. *)
  let redirect_or_escape (sym : B.Lang.Symbol.t) : B.Lang.Symbol.t B.Ctx.t =
    get_existing_redirect_or sym ~f:(Fn.compose B.Ctx.return Escape.escape)

  let escape_symbols (progs : B.Lang.Program.t list) :
      B.Lang.Program.t list B.Ctx.t =
    B.Ctx.Let_syntax.(
      (* We do this to make sure that every redirectable symbol known to the
         sanitiser is escaped, not just the ones that appear in the program. *)
      let%bind () = add_escape_redirects in
      (* That said, we do need to make sure that every symbol---not just the
         redirected ones---is escaped. As a result, we escape any symbol
         that the above redirection didn't catch. *)
      over_all_symbols ~f:redirect_or_escape progs)

  let get_symbols_in_use =
    B.Ctx.(get_symbol_table >>| Act_abstract.Symbol.Table.set)

  let first_unused_symbol used_set candidate_set =
    B.Lang.Symbol.Set.find candidate_set ~f:(fun candidate ->
        not
          (Act_abstract.Symbol.Set.mem used_set
             (B.Lang.Symbol.abstract candidate)) )

  let actually_unmangle (sym : B.Lang.Symbol.t) : B.Lang.Symbol.t B.Ctx.t =
    B.Ctx.Let_syntax.(
      let%bind valid_vars = B.Ctx.get_variables
      and possible_sym_vars = B.Ctx.get_redirect_sources sym
      and symbols_in_use = get_symbols_in_use in
      let candidates =
        B.Lang.Symbol.Set.inter valid_vars possible_sym_vars
      in
      let herd_safe_candidates =
        B.Lang.Symbol.(Set.filter ~f:is_herd_safe) candidates
      in
      match first_unused_symbol symbols_in_use herd_safe_candidates with
      | Some sym' ->
          B.Ctx.(redirect ~src:sym ~dst:sym' >>| fun () -> sym')
      | None ->
          B.Ctx.return sym)

  let unmangle =
    (* This is important, because trying to unmangle a symbol twice will
       fail---the first unmangling will register as the symbol being 'in
       use'. *)
    get_existing_redirect_or ~f:actually_unmangle

  let unmangle_symbols = over_all_symbols ~f:unmangle

  let run : B.Lang.Program.t list -> B.Lang.Program.t list B.Ctx.t =
    B.Ctx.(
      guard ~on:`Unmangle_symbols unmangle_symbols
      >=> guard ~on:`Escape_symbols escape_symbols)
end
