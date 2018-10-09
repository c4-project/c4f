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
open Lang

type t =
  { vf    : Format.formatter
  ; wf    : Format.formatter
  ; cid   : string
  ; spec  : CompilerSpec.t
  ; iname : string
  ; inp   : In_channel.t
  ; outp  : Out_channel.t
  ; mode  : [`Explain | `Litmusify]
  }

let lang_of_x86_dialect =
  function
  | X86Dialect.Att -> (module X86.ATT : X86.Lang)
  | X86Dialect.Intel -> (module X86.Intel : X86.Lang)
  | X86Dialect.Herd7 -> (module X86.Herd7 : X86.Lang)

let frontend_of_x86_dialect =
  function
  | X86Dialect.Att
    -> Or_error.return (module X86.AttFrontend : Lang.LangFrontend.Intf with type ast = X86Ast.t)
  | d ->
    Or_error.error "x86 dialect unsupported" d [%sexp_of: X86Dialect.t]

let parse_x86 dialect t =
  let open Or_error.Let_syntax in
  let%bind f = frontend_of_x86_dialect dialect in
  let module F = (val f : Lang.LangFrontend.Intf with type ast = X86Ast.t) in
  Or_error.tag_arg
    (F.run_ic ~file:t.iname t.inp)
    "assembly parse error" t.iname String.sexp_of_t

let output_litmus (type s)
    (module S : Sanitiser.Intf with type statement = s)
    (module L : Litmus.Intf with type LS.Statement.t = s)
    t
    (stms : s list)
    (conv : s list -> s list)
  =
  let emit_warnings =
  function
  | [] -> ()
  | ws ->
    let pp_warning f w =
      Format.fprintf f "@[<h>-@ @[<hov>%a@]@]@,"
        S.Warn.pp w
    in
    Format.fprintf t.wf "Warnings@ for@ %s:@ @[<v>%a@]@."
      t.iname
      (fun f -> List.iter ~f:(pp_warning f)) ws
  in
  let open Or_error.Let_syntax in
  let {S.programs; warnings} = S.sanitise stms in
  emit_warnings warnings;
  let%bind lit =
    Or_error.tag ~tag:"couldn't build litmus file"
    ( L.make ~name:t.iname
             ~init:[]
             ~programs:(List.map ~f:conv programs)
    )
  in
  let f = Format.formatter_of_out_channel t.outp in
  L.pp f lit;
  Format.pp_print_flush f ();
  Result.ok_unit

let output_explanation
    (type s)
    (module E : Explainer.S with type statement = s)
    t
    (program : s list) =
  let exp = E.explain program in
  let f = Format.formatter_of_out_channel t.outp in
  E.pp f exp;
  Format.pp_print_flush f ();
  Result.ok_unit

let run_x86 dialect t =
  (* TODO (@MattWindsor91): there must be a nicer way of generalising
     this. *)
  let open Result.Let_syntax in
  let module X = (val (lang_of_x86_dialect dialect) : X86.Lang) in
  let module S = X86Specifics.Sanitiser (X) in
  let module C = X86Conv.Make (X) (X86.Herd7) in
  let%bind asm = parse_x86 dialect t in
  match t.mode with
  | `Litmusify ->
    let module L = X86Specifics.LitmusDirect in
    output_litmus
      (module S : Sanitiser.Intf with type statement = X86Ast.statement)
      (module L : Litmus.Intf with type LS.Statement.t = X86Ast.statement)
      t
      (asm.program)
      C.convert
  | `Explain ->
    let module E = Explainer.Make (X) in
    output_explanation
      (module E : Explainer.S with type statement = X86Ast.statement)
      t
      (asm.program)

let run t =
  (* TODO (@MattWindsor91): eventually modularise the different
     languages, once we actually have some. *)
  match t.spec.emits with
  | Language.X86 dialect ->
    run_x86 dialect t
