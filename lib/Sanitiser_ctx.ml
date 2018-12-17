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

include Sanitiser_ctx_intf

let freshen_label (syms : Abstract.Symbol.Set.t) (prefix : string) : string =
  let rec mu prefix count =
    let str = sprintf "%s%d" prefix count in
    if Abstract.Symbol.Set.mem syms str
    then mu prefix (count + 1)
    else str
  in
  mu prefix 0
;;

module Make (Lang : Language.S) : S with module Lang := Lang = struct
  module Lang = Lang
  module Warn = Sanitiser_warn.Make (Lang)
  module Pass = Sanitiser_pass

  type ctx =
    { progname  : string
    ; endlabel  : string option
    ; syms      : Abstract.Symbol.Table.t
    ; variables : Lang.Symbol.Set.t
    ; redirects : Lang.Symbol.R_map.t
    ; passes    : Pass.Set.t
    ; warnings  : (string, Warn.t) List.Assoc.t
    }[@@deriving fields]

  let initial ~passes ~variables =
    { progname  = "(no program)"
    ; endlabel  = None
    ; syms      = Abstract.Symbol.Table.empty
    ; variables
    ; redirects = Lang.Symbol.R_map.make Lang.Symbol.Set.empty
    ; passes
    ; warnings  = []
    }

  include Travesty.State_transform.Make (struct
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
            { ctx with endlabel = Some lbl }
    )
  ;;

  let get_prog_name = peek progname

  let (|->) pass f a =
    let open Let_syntax in
    if%bind is_pass_enabled pass then f a else return a
  ;;

  let warn element body =
    modify (
      fun ctx ->
        let warning = Warn.make ~program_name:ctx.progname ~element ~body in
        { ctx with warnings = (ctx.progname, warning)::ctx.warnings }
    )
  ;;

  let warn_if predicate element body =
    when_m predicate ~f:(fun () -> warn element body)
  ;;

  let take_warnings program_name =
    make (
      fun ctx ->
        let prog_warnings, rest_warnings =
          List.partition_map ctx.warnings
            ~f:(fun (pn, warn) ->
                if String.equal pn program_name
                then `Fst warn
                else `Snd (pn, warn))
        in
        ( { ctx with warnings = rest_warnings }, prog_warnings )
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
    | Lang.Symbol.R_map.Identity -> sym
    | MapsTo sym' -> sym'
  ;;

  let get_variables = peek variables

  let get_redirect sym =
    let open Let_syntax in
    let%map rds = peek redirects in
    Option.map (Lang.Symbol.R_map.dest_of rds sym)
      ~f:(resolve_redirect sym)
  ;;

  let get_redirect_sources sym =
    let open Let_syntax in
    let%map rds = peek redirects in
    Lang.Symbol.R_map.sources_of rds sym
  ;;

  let get_redirect_alist syms
    : ((Lang.Symbol.t, Lang.Symbol.t) List.Assoc.t) t =
    let open Let_syntax in
    let%map rds = peek redirects in
    List.filter_map syms
      ~f:(fun sym ->
          Option.(
            Lang.Symbol.R_map.dest_of rds sym
            >>| resolve_redirect sym
            >>| Tuple2.create sym
          )
        )
    ;;

  let get_all_redirect_targets : Lang.Symbol.Set.t t =
    let open Let_syntax in
    let%map rds = peek redirects in
    Lang.Symbol.R_map.all_dests rds
  ;;

  let redirect ~src ~dst =
    let open Let_syntax in
    let%bind rds = peek redirects in
    (* Redirection can fail, which _should_ be an internal bug, so
       we make it a fatal error in the sanitiser. *)
    Monadic.modify
      (fun ctx ->
         let open Or_error.Let_syntax in
         let%map rds' = Lang.Symbol.R_map.redirect ~src ~dst rds in
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
