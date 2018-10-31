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
  type t =
    { o       : OutputCtx.t
    ; iname   : string
    ; inp     : In_channel.t
    ; outp    : Out_channel.t
    ; mode    : [`Explain | `Litmusify]
    ; passes  : Sanitiser_pass.Set.t
    ; symbols : string list
    }
  ;;

  val run : t -> (string, string) List.Assoc.t Or_error.t
end

module type Basic = sig
  module Frontend : LangFrontend.Intf
  module Litmus : Litmus.Intf
  module Multi_sanitiser
    : Sanitiser.S with type statement = Litmus.LS.Statement.t
                   and type sym = Litmus.LS.Symbol.t
                   and type 'a Program_container.t = 'a list
  ;;
  module Single_sanitiser
    : Sanitiser.S with type statement = Litmus.LS.Statement.t
                   and type sym = Litmus.LS.Symbol.t
                   and type 'a Program_container.t = 'a
  ;;
  module Explainer
    : Explainer.S with type statement = Litmus.LS.Statement.t
  ;;

  val final_convert : Litmus.LS.Statement.t list -> Litmus.LS.Statement.t list

  val statements : Frontend.ast -> Litmus.LS.Statement.t list
end

module Make (B : Basic) : S = struct
  type t =
    { o     : OutputCtx.t
    ; iname : string
    ; inp   : In_channel.t
    ; outp  : Out_channel.t
    ; mode  : [`Explain | `Litmusify]
    ; passes  : Sanitiser_pass.Set.t
    ; symbols : string list
    }
  ;;

  (* Shorthand for modules we use a _lot_. *)
  module L  = B.Litmus;;
  module LS = L.LS;;
  module MS = B.Multi_sanitiser;;
  module SS = B.Single_sanitiser;;
  module E  = B.Explainer;;

  let parse t =
    Or_error.tag_arg
      (B.Frontend.load_from_ic ~path:t.iname t.inp)
      "Error while parsing assembly" t.iname String.sexp_of_t
  ;;

  let make_init (progs : LS.Statement.t list list) : (string, LS.Constant.t) List.Assoc.t =
    let get_hsyms prog =
      prog
      |> LS.symbols
      |> Fn.flip Abstract.Symbol.Table.set_of_sort Abstract.Symbol.Sort.Heap
    in
    let syms = Abstract.Symbol.Set.union_list (List.map ~f:get_hsyms progs) in
    List.map ~f:(fun s -> (s, LS.Constant.zero))
      (Abstract.Symbol.Set.to_list syms)
  ;;

  let stringify_redirects =
    List.map
      ~f:(fun (k, v) -> (LS.Symbol.to_string k, LS.Symbol.to_string v))
  ;;

  let output_litmus
      t
      (symbols : LS.Symbol.t list)
      (stms : LS.Statement.t list)
      (conv : LS.Statement.t list -> LS.Statement.t list) =
    let emit_warnings =
      function
      | [] -> ()
      | ws ->
        let pp_warning f w =
          Format.fprintf f "@[<h>-@ @[<hov>%a@]@]@,"
            MS.Warn.pp w
        in
        Format.fprintf t.o.wf "Warnings@ for@ %s:@ @[<v>%a@]@."
          t.iname
          (fun f -> List.iter ~f:(pp_warning f)) ws
    in
    let open Or_error.Let_syntax in
    let%bind o = MS.sanitise ~passes:t.passes ~symbols stms in
    let programs = MS.Output.result o in
    let warnings = MS.Output.warnings o in
    emit_warnings warnings;
    let%map lit =
      Or_error.tag ~tag:"Couldn't build litmus file."
        ( L.make ~name:t.iname
            ~init:(make_init programs)
            ~programs:(List.map ~f:conv programs)
        )
    in
    let f = Format.formatter_of_out_channel t.outp in
    L.pp f lit;
    Format.pp_print_flush f ();
    stringify_redirects (MS.Output.redirects o)
  ;;

  let output_explanation
      t
      (symbols : LS.Symbol.t list)
      (program : LS.Statement.t list) =
    let open Or_error.Let_syntax in
    let%map san = SS.sanitise ~passes:t.passes ~symbols program in
    let exp = E.explain (SS.Output.result san) in
    let f = Format.formatter_of_out_channel t.outp in
    E.pp f exp;
    Format.pp_print_flush f ();
    stringify_redirects (SS.Output.redirects san)
  ;;

  let run t =
    (* TODO (@MattWindsor91): there must be a nicer way of generalising
       this. *)
    let open Result.Let_syntax in
    let%bind asm = parse t in
    let%bind symbols =
      t.symbols
      |> List.map
        ~f:(fun s -> Result.of_option (LS.Symbol.of_string_opt s)
               ~error:(
                 Error.create_s
                   [%message "Symbol can't be converted from string"
                       ~symbol:s]
               )
           )
      |> Or_error.combine_errors
    in
    match t.mode with
    | `Litmusify ->
      output_litmus
        t
        symbols
        (B.statements asm)
        B.final_convert
    | `Explain ->
      output_explanation t symbols (B.statements asm)
end
