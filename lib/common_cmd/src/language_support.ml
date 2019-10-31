(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

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

module Resolve_compiler = Act_machine.Resolver.Compiler (struct
  let f (cspec : C_spec.With_id.t) =
    let style = C_spec.With_id.style cspec in
    List.Assoc.find ~equal:Id.equal style_modules style
    |> Result.of_option
         ~error:
           (Error.create_s
              [%message "Unknown compiler style" ~style:(style : Id.t)])
end)
