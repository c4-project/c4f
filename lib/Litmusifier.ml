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

module type Intf = sig
  type t =
    { o      : OutputCtx.t
    ; iname  : string
    ; inp    : In_channel.t
    ; outp   : Out_channel.t
    ; mode   : [`Explain | `Litmusify]
    ; passes : Sanitiser_pass.Set.t
    }
  ;;

  val run : t -> unit Or_error.t
end

module type S = sig
  type statement

  module Frontend  : LangFrontend.Intf
  module Litmus    : (Litmus.Intf with type LS.Statement.t = statement)
  module Sanitiser : (Sanitiser.Intf with type statement = statement)
  module Explainer : (Explainer.S with type statement = statement)

  val final_convert : statement list -> statement list

  val statements : Frontend.ast -> statement list
end

module Make (M : S) : Intf = struct
  type t =
    { o     : OutputCtx.t
    ; iname : string
    ; inp   : In_channel.t
    ; outp  : Out_channel.t
    ; mode  : [`Explain | `Litmusify]
    ; passes : Sanitiser_pass.Set.t
    }
  ;;

  (* Shorthand for modules we use a _lot_. *)
  module L = M.Litmus;;
  module LS = L.LS;;
  module S = M.Sanitiser;;
  module E = M.Explainer;;

  let parse t =
    Or_error.tag_arg
      (M.Frontend.load_from_ic ~path:t.iname t.inp)
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

  let output_litmus
      t
      (stms : LS.Statement.t list)
      (conv : LS.Statement.t list -> LS.Statement.t list) =
    let emit_warnings =
      function
      | [] -> ()
      | ws ->
        let pp_warning f w =
          Format.fprintf f "@[<h>-@ @[<hov>%a@]@]@,"
            S.Warn.pp w
        in
        Format.fprintf t.o.wf "Warnings@ for@ %s:@ @[<v>%a@]@."
          t.iname
          (fun f -> List.iter ~f:(pp_warning f)) ws
    in
    let open Or_error.Let_syntax in
    let o = S.split_and_sanitise ~passes:t.passes stms in
    let programs = S.Output.result o in
    let warnings = S.Output.warnings o in
    emit_warnings warnings;
    let%bind lit =
      Or_error.tag ~tag:"Couldn't build litmus file."
        ( L.make ~name:t.iname
            ~init:(make_init programs)
            ~programs:(List.map ~f:conv programs)
        )
    in
    let f = Format.formatter_of_out_channel t.outp in
    L.pp f lit;
    Format.pp_print_flush f ();
    Result.ok_unit
  ;;

  let output_explanation
      t
      (program : LS.Statement.t list) =
    let san = S.sanitise ~passes:t.passes program in
    let exp = E.explain (S.Output.result san) in
    let f = Format.formatter_of_out_channel t.outp in
    E.pp f exp;
    Format.pp_print_flush f ();
    Result.ok_unit
  ;;

  let run t =
    (* TODO (@MattWindsor91): there must be a nicer way of generalising
       this. *)
    let open Result.Let_syntax in
    let%bind asm = parse t in
    match t.mode with
    | `Litmusify ->
      output_litmus
        t
        (M.statements asm)
        M.final_convert
    | `Explain ->
      output_explanation t (M.statements asm)
end
