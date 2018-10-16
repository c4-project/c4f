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
open Utils

let log_stage stage (o : OutputCtx.t) (paths : Pathset.t) cid =
  Format.fprintf o.vf "@[%s[%a]@ %s@]@."
    stage
    CompilerSpec.Id.pp cid
    paths.basename
;;

let compile o paths cid cspec =
  (* TODO(@MattWindsor91): inefficiently remaking the compiler module
     every time. *)
  let module C = (val Compiler.from_spec cid cspec) in
  log_stage "CC" o paths cid;
  Or_error.tag ~tag:"While compiling to assembly"
    (C.compile
       ~infile:paths.c_path
       ~outfile:(Pathset.compiler_asm_path paths cid))
;;

let litmusify o paths cid spec =
  let open Result.Let_syntax in
  log_stage "LITMUS" o paths cid;
  let%bind lit = LangSupport.get_litmusifier ~emits:spec.CompilerSpec.emits in
  let f src inp _ outp =
    let iname = MyFormat.format_to_string (Io.In_source.pp) src in
    let module L = (val lit) in
    L.run
      { o
      ; cid
      ; spec
      ; iname
      ; inp
      ; outp
      ; mode = `Litmusify
      ; passes = Sanitiser.Pass.all_set ()
      }
  in
  Or_error.tag ~tag:"While translating assembly to litmus"
    (Io.with_input_and_output
     (`File (Pathset.compiler_asm_path paths cid))
     (`File (Pathset.compiler_lita_path paths cid))
     ~f)
;;

(** [check_herd_output herd_path] checks to see if Herd wrote
    anything to [herd_path] and, if not, trips an error. *)
let check_herd_output herd_path =
  let f _ ic =
    (* TODO(@MattWindsor91): do something with these lines. *)
    match In_channel.input_lines ic with
    | [] ->
      Or_error.error_string
        ( "Herd doesn't seem to have outputted anything; "
          ^ "check the terminal for error output." )
    | _ -> Result.ok_unit
  in
  Io.In_source.with_input (`File herd_path) ~f
;;

(** [herd o paths cid spec] sees if [spec] asked for a Herd run
    on compiler [cid] and, if so, runs the requested Herd command
    on [cid]'s Litmus output. *)
let herd o paths cid (spec : CompilerSpec.t) =
  let open Or_error.Let_syntax in
  Option.value_map
    ~default:Result.ok_unit
    ~f:(
      fun prog ->
        log_stage "HERD" o paths cid;
        let f _ oc =
          Run.Local.run ~oc ~prog [ Pathset.compiler_lita_path paths cid ]
        in
        let herd_path = Pathset.compiler_herd_path paths cid in
        let%bind _ =
          Or_error.tag ~tag:"While running herd"
            (Io.Out_sink.with_output (`File herd_path) ~f)
        in
        check_herd_output herd_path
    )
    spec.herd
;;

let proc_c_on_compiler (o : OutputCtx.t) paths cid spec =
  let open Or_error.Let_syntax in
  let%bind _ = compile o paths cid spec in
  let%bind _ = litmusify o paths cid spec in
  herd o paths cid spec
;;

let proc_c (o : OutputCtx.t) specs ~in_root ~out_root c_fname =
  let open Or_error.Let_syntax in
  let%bind paths =
    Pathset.make_and_mkdirs specs ~out_root ~in_root ~c_fname
  in
  Pathset.pp o.vf paths;
  Format.pp_print_newline o.vf ();
  let enabled_specs = List.filter ~f:(fun (_, s) -> s.enabled) specs in
  let results =
    List.map
      ~f:(Tuple2.uncurry (proc_c_on_compiler o paths))
      enabled_specs
  in
  Or_error.combine_errors_unit results
;;

let is_c_file = MyFilename.has_extension ~ext:"c";;

let get_c_files (c_path : string) : string list Or_error.t =
  Or_error.(
    tag_arg
      (try_with (fun () -> Sys.readdir c_path))
      "Couldn't open directory: is this definitely a memalloy run?"
      c_path
      [%sexp_of: string]
    >>| Array.filter ~f:is_c_file
    >>| Array.to_list
  )
;;

let check_c_files_exist c_path c_files =
    if List.is_empty c_files
    then
      Or_error.error
        "Expected at least one C file." c_path [%sexp_of:string]
    else Result.ok_unit
;;

let report_spec_errors o =
  function
  | [] -> ()
  | es ->
    Format.fprintf o.OutputCtx.wf
      "@[<v>Some of the specified compilers don't seem to be valid:@,@,%a@]@."
      (Format.pp_print_list Error.pp ~pp_sep:Format.pp_print_cut)
      es
;;

let test_specs o specs =
  let (valid_specs, errors) = Compiler.test_specs specs in
  report_spec_errors o errors;
  Result.return valid_specs
;;

let run ?(local_only=false) ~in_root ~out_root o specs =
  let open Or_error.Let_syntax in
  let filtered_specs =
    if local_only
    then List.filter ~f:(fun (_, s) -> Option.is_none s.CompilerSpec.ssh) specs
    else specs
  in
  let%bind valid_specs = test_specs o filtered_specs in
  let c_path = Filename.concat in_root "C" in
  let%bind c_files = get_c_files c_path in
  let%bind _ = check_c_files_exist c_path c_files in
  let results = List.map ~f:(proc_c o valid_specs ~in_root ~out_root) c_files in
  Or_error.combine_errors_unit results
;;
