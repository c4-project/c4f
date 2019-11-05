(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the "Software"),
   to deal in the Software without restriction, including without limitation
   the rights to use, copy, modify, merge, publish, distribute, sublicense,
   and/or sell copies of the Software, and to permit persons to whom the
   Software is furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
   THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
   DEALINGS IN THE SOFTWARE. *)

open Base
module Asym = Act_abstract.Symbol

let freshen_label (syms : Set.M(Asym).t) (prefix : string) : string =
  let rec mu prefix count =
    let str = Printf.sprintf "%s%d" prefix count in
    if Set.mem syms str then mu prefix (count + 1) else str
  in
  mu prefix 0

module Err_ctx = Travesty.State_transform.Make2 (Or_error)

(** Parametrised inner state of the sanitiser context monad. *)
module State = struct
  type ('sset, 'smap, 'warn) t =
    { progname: string [@default "(no program)"]
    ; endlabel: string option
    ; syms: Asym.Table.t [@default Asym.Table.empty]
    ; variables: 'sset
    ; redirects: 'smap
    ; passes: Set.M(Pass_group).t
    ; warnings: (string, 'warn) List.Assoc.t [@default []] }
  [@@deriving fields, make]

  let pass_mem (ctx : ('a, 'b, 'c) t) ~(pass : Pass_group.t) : bool =
    Set.mem ctx.passes pass

  let set_end_label (ctx : ('a, 'b, 'c) t) ~(label : string) :
      ('a, 'b, 'c) t Or_error.t =
    match ctx.endlabel with
    | Some s ->
        Or_error.error_s
          [%message
            "Tried to overwrite end label" ~original:s ~replaced:label]
    | None ->
        Or_error.return {ctx with endlabel= Some label}

  let enter_program (ctx : ('a, 'b, 'c) t) ~(name : string) : ('a, 'b, 'c) t
      =
    {ctx with progname= name; endlabel= None; syms= Asym.Table.empty}

  let set_symbol_table (ctx : ('a, 'b, 'c) t) ~(syms : Asym.Table.t) :
      ('a, 'b, 'c) t =
    {ctx with syms}

  let add_symbol_to_table (ctx : ('a, 'b, 'c) t) ~(sym : Asym.t)
      ~(sort : Asym.Sort.t) : ('a, 'b, 'c) t =
    set_symbol_table ctx ~syms:(Asym.Table.add ctx.syms sym sort)
end

(** State monad functions that don't depend on the language definition. *)
module Common = struct
  let enter_program ~name : (unit, (_, _, _) State.t) Err_ctx.t =
    Err_ctx.(modify (State.enter_program ~name))

  let is_pass_enabled (pass : Pass_group.t) :
      (bool, (_, _, _) State.t) Err_ctx.t =
    Err_ctx.peek (State.pass_mem ~pass)

  let set_end_label (label : string) : (unit, (_, _, _) State.t) Err_ctx.t =
    Err_ctx.Monadic.modify (State.set_end_label ~label)

  let set_symbol_table (syms : Asym.Table.t) :
      (unit, (_, _, _) State.t) Err_ctx.t =
    Err_ctx.modify (State.set_symbol_table ~syms)

  let add_symbol_to_table (sym : Asym.t) (sort : Asym.Sort.t) :
      (unit, (_, _, _) State.t) Err_ctx.t =
    Err_ctx.modify (State.add_symbol_to_table ~sym ~sort)

  (* The () here is to get around the value restriction. *)
  let get_prog_name () : (string, (_, _, _) State.t) Err_ctx.t =
    Err_ctx.peek State.progname

  let get_end_label () : (string option, (_, _, _) State.t) Err_ctx.t =
    Err_ctx.peek State.endlabel

  let get_symbol_table () : (Asym.Table.t, (_, _, _) State.t) Err_ctx.t =
    Err_ctx.peek State.syms

  let get_variables () : ('svars, ('svars, _, _) State.t) Err_ctx.t =
    Err_ctx.peek State.variables
end

module Make (Lang : Act_language.Definition_types.S) :
  Ctx_types.S with module Lang := Lang = struct
  module Warn = Warn.Make (Lang.Element)

  type ctx = (Set.M(Lang.Symbol).t, Lang.Symbol.R_map.t, Warn.t) State.t

  let initial ~passes ~variables : ctx =
    let redirects = Lang.Symbol.R_map.identity () in
    State.make ~passes ~variables ~redirects ()

  module M =
    Travesty.State_transform.To_S
      (Err_ctx)
      (struct
        type t = ctx
      end)

  include M
  include Common

  (* These all have units in their 'common' definition, so we have to fix
     them up by applying them a bit. *)
  let get_prog_name = get_prog_name ()

  let get_end_label = get_end_label ()

  let get_symbol_table = get_symbol_table ()

  let get_variables = get_variables ()

  let guard (f : 'a -> 'a t) ~(on : Pass_group.t) (a : 'a) : 'a t =
    Let_syntax.(if%bind is_pass_enabled on then f a else return a)

  let warn element body =
    modify (fun ctx ->
        let warning = Warn.make ~program_name:ctx.progname ~element ~body in
        {ctx with warnings= (ctx.progname, warning) :: ctx.warnings})

  let warn_if predicate element body =
    when_m predicate ~f:(fun () -> warn element body)

  let take_warnings program_name =
    make (fun ctx ->
        let prog_warnings, rest_warnings =
          List.partition_map ctx.warnings ~f:(fun (pn, warn) ->
              if String.equal pn program_name then `Fst warn
              else `Snd (pn, warn))
        in
        ({ctx with warnings= rest_warnings}, prog_warnings))

  let get_symbols_with_sorts (sorts : Asym.Sort.t list) : Set.M(Asym).t t =
    let sort_set = Set.of_list (module Asym.Sort) sorts in
    Asym.(
      let open Let_syntax in
      let%map all_syms = get_symbol_table in
      Table.set_of_sorts all_syms sort_set)

  let get_redirect (sym : Lang.Symbol.t) : Lang.Symbol.t t =
    let open Let_syntax in
    let%map rds = peek State.redirects in
    Lang.Symbol.R_map.dest_of_sym rds sym

  let get_redirect_sources (sym : Lang.Symbol.t) : Set.M(Lang.Symbol).t t =
    let open Let_syntax in
    let%map rds = peek State.redirects in
    Lang.Symbol.R_map.sources_of_sym rds sym

  let get_redirect_alist syms : (Lang.Symbol.t, Lang.Symbol.t) List.Assoc.t t
      =
    let open Let_syntax in
    let%map rds = peek State.redirects in
    List.map syms ~f:(fun sym ->
        (sym, Lang.Symbol.R_map.dest_of_sym rds sym))

  let get_all_redirect_targets : Set.M(Lang.Symbol).t t =
    let open Let_syntax in
    let%bind rds = peek State.redirects in
    let%map sources = peek State.variables in
    Lang.Symbol.R_map.dest_syms rds ~sources

  let modify_rmap
      ~(f : Lang.Symbol.R_map.t -> Lang.Symbol.R_map.t Or_error.t) : unit t =
    let open Let_syntax in
    let%bind rds = peek State.redirects in
    (* Redirection can fail, which _should_ be an internal bug, so we make it
       a fatal error in the sanitiser. *)
    Monadic.modify (fun ctx ->
        let open Or_error.Let_syntax in
        let%map rds' = f rds in
        {ctx with redirects= rds'})

  (* TODO(@MattWindsor91): propagate changes to the abstract map *)

  let redirect ~src ~dst : unit t =
    modify_rmap
      ~f:(Fn.compose Or_error.return (Lang.Symbol.R_map.redirect ~src ~dst))

  let add_symbol (sym_name : Asym.t) (sort : Asym.Sort.t) : Asym.t t =
    let open Let_syntax in
    let%bind () = add_symbol_to_table sym_name sort in
    let%bind sym = Monadic.return (Lang.Symbol.require_of_string sym_name) in
    let%map () = redirect ~src:sym ~dst:sym in
    sym_name

  let make_fresh_symbol (prefix : string) ~(of_sort : Asym.Sort.t)
      ~(fresh_in_sorts : Asym.Sort.t list) : Asym.t t =
    Let_syntax.(
      let%bind syms = get_symbols_with_sorts fresh_in_sorts in
      let l = freshen_label syms prefix in
      add_symbol l of_sort)

  let make_fresh_label : string -> Asym.t t =
    make_fresh_symbol ~of_sort:Asym.Sort.Label
      ~fresh_in_sorts:Asym.Sort.[Jump; Label]

  let make_fresh_heap_loc : string -> Asym.t t =
    make_fresh_symbol ~of_sort:Asym.Sort.Heap
      ~fresh_in_sorts:Asym.Sort.[Heap]
end
