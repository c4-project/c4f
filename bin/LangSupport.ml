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
open Lib

let get_litmusifier_x86 =
  let open Or_error.Let_syntax in
  function
  | [s] ->
    let%bind dialect =
      Option.try_with (fun () -> X86.Dialect.of_string s)
      |> Result.of_option ~error:(Error.createf "Unknown X86 dialect: %s" s)
    in
    let%bind f = X86.Language.frontend_of_dialect dialect in
    let l = X86.Language.lang_of_dialect dialect in
    Or_error.return
      (module Litmusifier.Make (
          struct
            module L = (val l)

            type statement = L.Statement.t

            module Frontend = (val f)
            module Litmus = X86.Litmus.LitmusDirect
            module Sanitiser = X86.Sanitiser.Make (L)
            module Explainer = Explainer.Make (L)

            module Conv = X86.Conv.Make (L) (X86.Language.Herd7)

            let final_convert = Conv.convert
            let statements = X86.Ast.program
          end
          ): Litmusifier.Intf)
  | _ ->
    Or_error.error_string
      "Too many arguments to x86 language; expected only one."

let lang_procs =
  [ "x86", get_litmusifier_x86 ]
;;

let get_litmusifier ~emits =
  let open Or_error.Let_syntax in
  let%bind lang =
    List.hd emits
    |> Result.of_option ~error:(Error.of_string "Missing language name")
  in
  let%bind proc =
    List.Assoc.find ~equal:String.Caseless.equal lang_procs lang
    |> Result.of_option ~error:(Error.createf "Unknown language: %s" lang)
  in
  proc (List.tl_exn emits)
;;

module Gcc : Compiler.S = struct
  let compile_args ~args ~emits ~infile ~outfile =
    ignore emits;
    [ "-S"       (* emit assembly *)
    ; "-fno-pic" (* don't emit position-independent code *)
    ]
    @ args
    @ [ "-o"; outfile; infile]
  ;;

  let test_args = ["--version"] ;;
end

let style_modules =
  [ "gcc", (module Gcc : Compiler.S) ]
;;

let compiler_module_from_spec (cspec : Compiler.Spec.with_id) =
  let style = cspec.cspec.style in
  List.Assoc.find ~equal:String.Caseless.equal style_modules style
  |> Result.of_option ~error:(Error.createf "Unknown compiler style: %s" style)
;;

let compiler_from_spec (cspec : Compiler.Spec.with_id) =
  Compiler.from_spec
    compiler_module_from_spec
    cspec
;;

let test_spec cspec =
  let open Or_error.Let_syntax in
  let%bind m = compiler_from_spec cspec in
  let module M = (val m) in
  Or_error.tag_arg
    (M.test ())
    "A compiler in your spec file didn't respond properly"
    cspec.cid
    [%sexp_of:Compiler.Id.t]
;;

let test_specs specs = Compiler.Set.test ~f:test_spec specs;;
