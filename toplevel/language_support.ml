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

open Core_kernel

let lang_procs = [("x86", X86.Asm_job.get_runner)]

let try_get_lang_proc language =
  language
  |> List.Assoc.find ~equal:String.Caseless.equal lang_procs
  |> Result.of_option
       ~error:(Error.create_s [%message "Unknown language" ~language])

let asm_runner_from_arch (arch : Config.Id.t) =
  match Config.Id.to_string_list arch with
  | [] ->
      Or_error.error_string "Missing language name"
  | lang :: rest ->
      Result.(try_get_lang_proc lang >>= fun proc -> proc rest)

module Gcc : Config.Compiler.Basic = struct
  let compile_args ~args ~emits ~infile ~outfile =
    ignore emits ;
    [ "-S" (* emit assembly *)
    ; "-fno-pic"
      (* don't emit position-independent code *) ]
    @ args @ ["-o"; outfile; infile]

  let test_args = ["--version"]
end

let style_modules = [("gcc", (module Gcc : Config.Compiler.Basic))]

module Resolver :
  Config.Compiler.Basic_resolver
  with type spec := Config.Compiler.Spec.With_id.t = struct
  let resolve (cspec : Config.Compiler.Spec.With_id.t) =
    let style = Config.Compiler.Spec.With_id.style cspec in
    List.Assoc.find ~equal:String.Caseless.equal style_modules style
    |> Result.of_option
         ~error:(Error.create_s [%message "Unknown compiler style" ~style])
end

module Resolve_compiler = Config.Compiler.Make_resolver (Resolver)
module Resolve_compiler_from_target =
  Config.Compiler.Make_target_resolver (Resolver)

let test_compiler cspec =
  let open Or_error.Let_syntax in
  let%bind (module M) = Resolve_compiler.from_spec cspec in
  let%map () =
    Or_error.tag_arg (M.test ())
      "A compiler in your spec file didn't respond properly"
      (Config.Compiler.Spec.With_id.id cspec)
      [%sexp_of: Config.Id.t]
  in
  Some cspec

let filter_compiler predicate cspec =
  Option.some_if (Config.Compiler.Property.eval_b cspec predicate) cspec

let compiler_hook with_compiler_tests predicate cspec =
  match filter_compiler predicate cspec with
  | Some cspec' ->
      if with_compiler_tests then test_compiler cspec'
      else Or_error.return (Some cspec')
  | None ->
      Or_error.return None

let machine_hook predicate mspec =
  let eval_b =
    Config.Machine.Property.eval_b (module Config.Machine.Spec.With_id)
  in
  Or_error.return
    ((* TODO(@MattWindsor91): actually test the machine here! *)
     Option.some_if (eval_b mspec predicate) mspec)

let load_and_process_config ?(compiler_predicate = Blang.true_)
    ?(machine_predicate = Blang.true_)
    ?(sanitiser_passes = Blang.base `Default) ?(with_compiler_tests = true)
    (path : Fpath.t) =
  let open Or_error.Let_syntax in
  let%bind rcfg = Config.Act.Raw.load ~path in
  let chook = compiler_hook with_compiler_tests compiler_predicate in
  let mhook = machine_hook machine_predicate in
  let phook = Config.Sanitiser_pass.Selector.eval_b sanitiser_passes in
  Config.Act.from_raw rcfg ~chook ~mhook ~phook
