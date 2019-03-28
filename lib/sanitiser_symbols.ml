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

(** Sanitiser passes for global symbol renaming

    This module provides a global sanitiser pass that performs two
    renaming sub-passes on all symbols:

    - [`Unmangle_symbols]: replace compiler-mangled symbols
    with their original C identifiers where doing so is unambiguous;
    - [`Escape_symbols]: replace characters in symbols that are
    difficult for Herd-like programs to parse with less human-readable
    (but more machine-readable) equivalents. *)

module Make (B : Sanitiser_base.Basic) :
  Sanitiser_base.S_all
  with module Lang := B.Lang
   and module Ctx := B.Ctx
   and module Program_container := B.Program_container = struct
  include B
  module Ctx_Pcon = Program_container.On_monad (Ctx)
  module Ctx_Prog_Sym = Lang.Program.On_symbols.On_monad (Ctx)

  let over_all_symbols progs ~f =
    (* Nested mapping:
       over symbols in statements in statement lists in programs. *)
    Ctx_Pcon.map_m progs ~f:(Ctx_Prog_Sym.map_m ~f)
  ;;

  let get_existing_redirect_or sym ~f =
    let open Ctx.Let_syntax in
    let%bind sym' = Ctx.get_redirect sym in
    if Lang.Symbol.equal sym sym' then f sym else Ctx.return sym'
  ;;

  module Escape = Symbol_escape.Make (Lang.Symbol)

  let add_escape_redirects : unit Ctx.t =
    let open Ctx.Let_syntax in
    let%bind to_escape = Ctx.get_variables in
    Ctx.modify_rmap ~f:(Fn.compose Or_error.return (Escape.escape_rmap ~to_escape))
  ;;

  (** [redirect_or_escape sym] gets the redirected form of [sym], if
      one exists, or the escaped form of [sym] otherwise. *)
  let redirect_or_escape (sym : Lang.Symbol.t) : Lang.Symbol.t Ctx.t =
    get_existing_redirect_or sym ~f:(Fn.compose Ctx.return Escape.escape)
  ;;

  let escape_symbols (progs : Lang.Program.t Program_container.t)
      : Lang.Program.t Program_container.t Ctx.t
    =
    let open Ctx.Let_syntax in
    (* We do this to make sure that every redirectable symbol known to
       the sanitiser is escaped, not just the ones that appear in the
       program. *)
    let%bind () = add_escape_redirects in
    (* That said, we do need to make sure that every symbol---not just
       the redirected ones---is escaped.  As a result, we escape any
       symbol that the above redirection didn't catch. *)
    over_all_symbols ~f:redirect_or_escape progs
  ;;

  let get_symbols_in_use =
    let open Ctx.Let_syntax in
    let%map symbol_table = Ctx.get_symbol_table in
    Abstract.Symbol.Table.set symbol_table
  ;;

  let first_unused_symbol used_set candidate_set =
    Lang.Symbol.Set.find candidate_set ~f:(fun candidate ->
        not (Abstract.Symbol.Set.mem used_set (Lang.Symbol.abstract candidate)))
  ;;

  let actually_unmangle (sym : Lang.Symbol.t) : Lang.Symbol.t Ctx.t =
    let open Ctx.Let_syntax in
    let%bind valid_vars = Ctx.get_variables
    and possible_sym_vars = Ctx.get_redirect_sources sym
    and symbols_in_use = get_symbols_in_use in
    let candidates = Lang.Symbol.Set.inter valid_vars possible_sym_vars in
    let herd_safe_candidates = Lang.Symbol.(Set.filter ~f:is_herd_safe) candidates in
    match first_unused_symbol symbols_in_use herd_safe_candidates with
    | Some sym' -> Ctx.(redirect ~src:sym ~dst:sym' >>| fun () -> sym')
    | None -> Ctx.return sym
  ;;

  let unmangle =
    (* This is important, because trying to unmangle a symbol twice
       will fail---the first unmangling will register as the symbol
       being 'in use'. *)
    get_existing_redirect_or ~f:actually_unmangle
  ;;

  let unmangle_symbols = over_all_symbols ~f:unmangle

  let on_all progs =
    Ctx.(
      progs
      |> (`Unmangle_symbols |-> unmangle_symbols)
      >>= (`Escape_symbols |-> escape_symbols))
  ;;
end
