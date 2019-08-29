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
open Act_common

(* Module shorthand *)
module C_spec = Act_compiler.Spec
module M_spec = Act_machine.Spec
module Mw_spec = M_spec.With_id
module Cq_spec = Act_machine.Qualified.Compiler
module C_types = Act_compiler.Instance_types

(* TODO(@MattWindsor91): distinguish between x86 and x64 here. *)
let lang_procs =
  [("x86", Act_x86.Asm_job.get_runner); ("x64", Act_x86.Asm_job.get_runner)]

let try_get_lang_proc (language : string) =
  language
  |> List.Assoc.find ~equal:String.Caseless.equal lang_procs
  |> Result.of_option
       ~error:(Error.create_s [%message "Unknown language" ~language])

let asm_runner_of_arch :
    Id.t -> (module Act_asm.Runner_intf.Basic) Or_error.t =
  Id.hd_reduce
    ~on_empty:(fun () -> Or_error.error_string "Missing language name")
    ~f:(fun lang rest ->
      Result.(try_get_lang_proc lang >>= fun proc -> proc rest))

let asm_runner_of_target
    (tgt : Act_machine.Qualified.Compiler.t Act_machine.Target.t) :
    (module Act_asm.Runner_intf.Basic) Or_error.t =
  asm_runner_of_arch (Act_machine.Target.arch tgt)

let style_modules : (Id.t, (module C_types.Basic)) List.Assoc.t =
  [(Id.of_string "gcc", (module Act_compiler_gcc.Instance))]

module Resolver : Act_machine.Resolver.Basic = struct
  let resolve (cspec : C_spec.With_id.t) =
    let style = C_spec.With_id.style cspec in
    List.Assoc.find ~equal:Id.equal style_modules style
    |> Result.of_option
         ~error:
           (Error.create_s
              [%message "Unknown compiler style" ~style:(style : Id.t)])
end

module Resolve_compiler = Act_machine.Resolver.Make (Resolver)
module Resolve_compiler_from_target =
  Act_machine.Resolver.Make_on_target (Resolver)

let test_compiler (spec : Cq_spec.t) : Cq_spec.t option Or_error.t =
  let c_spec = Cq_spec.c_spec spec in
  Or_error.Let_syntax.(
    let%bind (module M) = Resolve_compiler.from_spec spec in
    let%map () =
      Or_error.tag_arg (M.test ())
        "A compiler in your spec file didn't respond properly"
        (C_spec.With_id.id c_spec)
        [%sexp_of: Id.t]
    in
    Some spec)

let filter_compiler predicate (spec : Cq_spec.t) : Cq_spec.t option =
  let cspec = Cq_spec.c_spec spec in
  Option.some_if (Act_compiler.Property.eval_b cspec predicate) spec

let compiler_hook with_compiler_tests predicate (spec : Cq_spec.t) :
    Cq_spec.t option Or_error.t =
  match filter_compiler predicate spec with
  | Some cspec' ->
      if with_compiler_tests then test_compiler cspec'
      else Or_error.return (Some cspec')
  | None ->
      Or_error.return None

let machine_hook predicate (mspec : Mw_spec.t) : Mw_spec.t option Or_error.t
    =
  let eval_b = Act_machine.Property.eval_b in
  Or_error.return
    (* TODO(@MattWindsor91): actually test the machine here! *)
    (Option.some_if (eval_b mspec predicate) mspec)

let make_filtered_machine_config
    ?(compiler_predicate : Act_compiler.Property.t Blang.t = Blang.true_)
    ?(machine_predicate : Act_machine.Property.t Blang.t = Blang.true_)
    ?(with_compiler_tests : bool = false) (global : Act_config.Global.t) :
    Act_config.Act.t Or_error.t =
  let chook = compiler_hook with_compiler_tests compiler_predicate in
  let mhook = machine_hook machine_predicate in
  Act_config.Act.of_global global ~chook ~mhook
