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

type herd_outcome =
  [ Herd.outcome | `Disabled | `Errored ]
[@@deriving sexp]
;;

(** [herd_result] summarises the result of running Herd. *)
type herd_result =
  { result       : herd_outcome
  ; final_states : int sexp_option
  }
[@@deriving sexp]
;;

type run_result =
  { herd       : herd_result
  }
[@@deriving sexp]
;;

type compiler_result = (string, run_result) List.Assoc.t
[@@deriving sexp]
;;

type full_result = (Compiler.Id.t, compiler_result) List.Assoc.t
[@@deriving sexp]
;;

let log_stage stage (o : OutputCtx.t) (fs : Pathset.File.t) cid =
  Format.fprintf o.vf "@[%s[%a]@ %s@]@."
    stage
    Compiler.Id.pp cid
    (Pathset.File.basename fs)
;;

let compile o fs cspec =
  let open Or_error.Let_syntax in
  (* TODO(@MattWindsor91): inefficiently remaking the compiler module
     every time. *)
  let%bind c = LangSupport.compiler_from_spec cspec in
  let cid = Compiler.CSpec.WithId.id cspec in
  let module C = (val c) in
  log_stage "CC" o fs cid;
  Or_error.tag ~tag:"While compiling to assembly"
    (C.compile
       ~infile:(Pathset.File.c_path fs)
       ~outfile:(Pathset.File.asm_path fs))
;;

let litmusify o fs cspec =
  let cid = Compiler.CSpec.WithId.id cspec in
  log_stage "LITMUS" o fs cid;
  Or_error.tag ~tag:"While translating assembly to litmus"
    (Common.do_litmusify
       `Litmusify
       (Sanitiser.Pass.all_set ())
       o
       ~infile:(Some (Pathset.File.asm_path fs))
       ~outfile:(Some (Pathset.File.lita_path fs))
       (Compiler.CSpec.WithId.spec cspec))
;;

(** [check_herd_output path] runs analysis on the Herd output at
   [path]. *)
let check_herd_output (o : OutputCtx.t) path : herd_result =
  match Herd.load ~path with
  | Result.Ok herd ->
    { result       = (Herd.single_outcome_of herd :> herd_outcome)
    ; final_states = Some (List.length (Herd.states herd))
    }
  | Result.Error err ->
    Format.fprintf o.wf "@[<v 4>Herd analysis error:@,%a@]@."
      Error.pp err;
    { result = `Errored; final_states = None }
;;

(** [herd o fs cspec] sees if [cspec] asked for a Herd run and, if
   so, runs the requested Herd command on its Litmus output. *)
let herd o fs (cspec : Compiler.CSpec.WithId.t) =
  let open Or_error.Let_syntax in
  Option.value_map
    ~default:(return { result = `Disabled; final_states = None })
    ~f:(
      fun prog ->
        let cid = Compiler.CSpec.WithId.id cspec in
        log_stage "HERD" o fs cid;
        let f _ oc =
          Run.Local.run ~oc ~prog [ Pathset.File.lita_path fs ]
        in
        let herd_path = Pathset.File.herd_path fs in
        let%bind () =
          Or_error.tag ~tag:"While running herd"
            (Io.Out_sink.with_output (`File herd_path) ~f)
        in
        return (check_herd_output o herd_path)
    )
    (Compiler.CSpec.herd
       (Compiler.CSpec.WithId.spec cspec))
;;

let run_single (o : OutputCtx.t) (ps: Pathset.t) spec fname =
  let basename = Filename.chop_extension
      (Filename.basename fname)
  in
  let fs = Pathset.File.make ps basename in
  let open Or_error.Let_syntax in
  let%bind () = compile o fs spec in
  let%bind () = litmusify o fs spec in
  let%bind herd = herd o fs spec in
  return (basename, { herd });
;;

let run_compiler (o : OutputCtx.t) ~in_root ~out_root c_fnames spec
  : (Compiler.Id.t * compiler_result) Or_error.t =
  let open Or_error.Let_syntax in
  let id = Compiler.CSpec.WithId.id spec in
  let%bind paths = Pathset.make_and_mkdirs id ~in_root ~out_root in
  Pathset.pp o.vf paths;
  Format.pp_print_newline o.vf ();

  let%bind results =
    c_fnames
    |> List.sort ~compare:String.compare
    |> List.map ~f:(run_single o paths spec)
    |> Or_error.combine_errors
  in
  return (id, results)
;;

let check_c_files_exist c_path c_files =
    if List.is_empty c_files
    then
      Or_error.error
        "Expected at least one C file." c_path [%sexp_of:string]
    else Result.ok_unit
;;

let report_spec_errors o = function
  | [] -> ()
  | es ->
    Format.fprintf o.OutputCtx.wf
      "@[<v>Some of the specified compilers don't seem to be valid:@,@,%a@]@."
      (Format.pp_print_list Error.pp ~pp_sep:Format.pp_print_cut)
      es
;;

let print_result (r : full_result) =
  Format.open_box 0;
  Sexp.pp_hum Format.std_formatter [%sexp (r : full_result)];
  Format.close_box ();
  Format.print_newline ()
;;

let run ~in_root ~out_root o cfg =
  let open Or_error.Let_syntax in

  let specs = Config.M.compilers cfg in
  report_spec_errors o (List.filter_map ~f:snd (Config.M.disabled_compilers cfg));

  let c_path = Filename.concat in_root "C" in
  let%bind c_files = Io.Dir.get_files c_path ~ext:"c" in
  let%bind () = check_c_files_exist c_path c_files in

  let results = Compiler.CSpec.Set.map ~f:(run_compiler o ~in_root ~out_root c_files) specs in
  let%bind result = Or_error.combine_errors results in
  print_result result;
  return ()
;;

let command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"runs automatic testing over a memalloy output directory"
    [%map_open
      let spec_file =
        flag "spec"
          (optional_with_default
             (Filename.concat Filename.current_dir_name "compiler.spec")
             string)
          ~doc: "PATH the compiler spec file to use"
      and out_root =
        flag "output"
          (optional_with_default
             Filename.current_dir_name
             string)
          ~doc: "PATH the path under which output directories will be created"
      and verbose =
        flag "verbose"
          no_arg
          ~doc: "verbose mode"
      and no_warnings =
        flag "no-warnings"
          no_arg
          ~doc: "silence all warnings"
      and local_only =
        flag "local-only"
          no_arg
          ~doc: "skip all remote compilers"
      and in_root =
        anon ("RESULTS_PATH" %: string)
      in
      fun () ->
        let warnings = not no_warnings in
        let o = OutputCtx.make ~verbose ~warnings in
        Result.Let_syntax.(
          let%bind cfg = LangSupport.load_cfg ~local_only spec_file in
          run o cfg ~in_root ~out_root
        ) |> Common.print_error
    ]
;;
