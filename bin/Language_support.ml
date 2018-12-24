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
open Lib

let try_get_x86_dialect dialect =
  dialect
  |> X86.Dialect.Name_table.of_string
  |> Result.of_option
    ~error:(Error.create_s [%message "Unknown X86 dialect" ~dialect])
;;

let get_runner_x86 = function
  | [ dialect_s ] ->
    let open Or_error.Let_syntax in
    let%bind dialect = try_get_x86_dialect dialect_s in
    let%map (module Frontend) = X86.Frontend.of_dialect dialect in
    let (module Lang) = X86.Language.of_dialect dialect in
    (module Asm_job.Make_runner (struct
         type ast = X86.Ast.t

         module Src_lang = Lang
         module Dst_lang = X86.Language.Herd7

         module Frontend = Frontend
         module Litmus_ast = Litmus.Ast.Make (Dst_lang)
         module Litmus_pp = Litmus.Pp.Make_tabular (struct
             include Litmus_ast
             module Lang = Dst_lang
           end)
         module Multi_sanitiser = X86.Sanitiser.Make_multi (Src_lang)
         module Single_sanitiser = X86.Sanitiser.Make_single (Src_lang)
         module Explainer = Explainer.Make (Src_lang)

         module Conv = X86.Conv.Make (Src_lang) (Dst_lang)

         let final_convert = Conv.convert
         let statements = X86.Ast.program
       end): Asm_job.Runner)
  | [] ->
    Or_error.error_string
      "Not enough arguments to x86 language; expected one."
  | _::_ ->
    Or_error.error_string
      "Too many arguments to x86 language; expected only one."
;;

let lang_procs =
  [ "x86", get_runner_x86 ]
;;

let try_get_lang_proc language =
  language
  |> List.Assoc.find ~equal:String.Caseless.equal lang_procs
  |> Result.of_option ~error:(
    Error.create_s [%message "Unknown language" ~language]
  )
;;

let asm_runner_from_emits = function
  | [] -> Or_error.error_string "Missing language name"
  | (lang::rest) ->
    Result.(try_get_lang_proc lang >>= (fun proc -> proc rest))
;;

module Gcc : Compiler.Basic = struct
  let compile_args ~args ~emits ~infile ~outfile =
    ignore emits;
    [ "-S"       (* emit assembly *)
    ; "-fno-pic" (* don't emit position-independent code *)
    ]
    @ args
    @ [ "-o"; outfile; infile]
  ;;

  let test_args = ["--version"]
end

let style_modules =
  [ "gcc", (module Gcc : Compiler.Basic) ]
;;

let compiler_module_from_spec (cspec : Compiler.Spec.With_id.t) =
  let style = Compiler.Spec.With_id.style cspec in
  List.Assoc.find ~equal:String.Caseless.equal style_modules style
  |> Result.of_option
    ~error:(Error.create_s
              [%message "Unknown compiler style" ~style]
           )
;;

let asm_runner_from_spec (cspec : Compiler.Spec.With_id.t) =
  let emits = Compiler.Spec.With_id.emits cspec in
  asm_runner_from_emits emits
;;

let compiler_from_spec (cspec : Compiler.Spec.With_id.t) =
  Compiler.from_spec compiler_module_from_spec cspec
;;

let test_compiler cspec =
  let open Or_error.Let_syntax in
  let%bind (module M) = compiler_from_spec cspec in
  let%map () =
    Or_error.tag_arg
      (M.test ())
      "A compiler in your spec file didn't respond properly"
      (Compiler.Spec.With_id.id cspec)
      [%sexp_of:Id.t]
  in
  Some cspec
;;

let filter_compiler predicate cspec =
  Option.some_if
    (Compiler.Property.eval_b cspec predicate)
    cspec
;;

let compiler_hook with_compiler_tests predicate cspec =
  match filter_compiler predicate cspec with
  | Some cspec' ->
    if with_compiler_tests
    then test_compiler cspec'
    else Or_error.return (Some cspec')
  | None ->
    Or_error.return None
;;


let machine_hook predicate mspec =
  let eval_b =
    Machine.Property.eval_b (module Machine.Spec.With_id)
  in
  Or_error.return (
    (* TODO(@MattWindsor91): actually test the machine here! *)
    Option.some_if (eval_b mspec predicate) mspec
  )
;;

let load_and_process_config
    ?(compiler_predicate=Blang.true_)
    ?(machine_predicate=Blang.true_)
    ?(sanitiser_passes=Blang.base `Default)
    ?(with_compiler_tests=true) path =
  let open Or_error.Let_syntax in
  let%bind rcfg = Config.Raw.load ~path in
  let chook = compiler_hook with_compiler_tests compiler_predicate in
  let mhook = machine_hook machine_predicate in
  let phook = Sanitiser_pass.Selector.eval_b sanitiser_passes in
  Config.M.from_raw rcfg ~chook ~mhook ~phook
;;
