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

(** [herd_run_result] summarises the result of running Herd. *)
type herd_run_result =
  [ `Success of Herd.t
  | `Disabled
  | `Errored
  ]
;;

(** [herd_analysis] summarises the result of running Herd on a C program
    and its assembly, then comparing the results. *)
type herd_analysis =
  [ Herd.outcome
  | `Disabled
  | `Errored of [`C | `Assembly]
  ]
[@@deriving sexp]
;;

type run_result =
  { herd       : herd_analysis
  ; time_taken : Time.Span.t
  }
[@@deriving sexp]
;;

type compiler_result = (string, run_result) List.Assoc.t
[@@deriving sexp]
;;

type full_result = (Spec.Id.t, compiler_result) List.Assoc.t
[@@deriving sexp]
;;

let log_stage stage (o : OutputCtx.t) name cid =
  Format.fprintf o.vf "@[%s[%a]@ %s@]@."
    stage
    Spec.Id.pp cid
    name
;;

let compile o fs cspec =
  let open Or_error.Let_syntax in
  (* TODO(@MattWindsor91): inefficiently remaking the compiler module
     every time. *)
  let%bind c = LangSupport.compiler_from_spec cspec in
  let cid = Compiler.Full_spec.With_id.id cspec in
  let module C = (val c) in
  log_stage "CC" o (Pathset.File.basename fs) cid;
  Or_error.tag ~tag:"While compiling to assembly"
    (C.compile
       ~infile:(Pathset.File.c_path fs)
       ~outfile:(Pathset.File.asm_path fs))
;;

(** [check_herd_output path] runs analysis on the Herd output at
   [path]. *)
let check_herd_output (o : OutputCtx.t) path : herd_run_result =
  match Herd.load ~path with
  | Result.Ok herd -> `Success herd
  | Result.Error err ->
    Format.fprintf o.wf "@[<v 4>Herd analysis error:@,%a@]@."
      Error.pp err;
    `Errored
;;

(** [herd o prog ~infile ~outfile cspec] sees if [cspec] asked for a
   Herd run and, if so, runs the Herd command [prog] on [infile],
    outputting to [outfile]. *)
let herd o prog ~infile ~outfile (cspec : Compiler.Full_spec.With_id.t) =
  let (id, spec) = Compiler.Full_spec.With_id.to_tuple cspec in
  let open Or_error.Let_syntax in
  if Compiler.Full_spec.herd spec
  then begin
    log_stage "HERD" o infile id;
    let f _ oc = Run.Local.run ~oc ~prog [ infile ] in
    let%map () =
      Or_error.tag ~tag:"While running herd"
        (Io.Out_sink.with_output (`File outfile) ~f)
    in
    check_herd_output o outfile
  end else return `Disabled
;;

(** [lower_thread_local_symbol] converts thread-local symbols of the
   form `0:r0` into the memalloy witness equivalent, `t0r0`. *)
let lower_thread_local_symbol sym =
  Option.value ~default:sym
    ( let open Option.Let_syntax in
      let%bind (thread, rest) = String.lsplit2 ~on:':' sym in
      let%bind tnum = Caml.int_of_string_opt thread in
      let%map tnum = Option.some_if (Int.is_non_negative tnum) tnum in
      sprintf "t%d%s" tnum rest
    )
;;

let analyse
    (c_herd : herd_run_result)
    (a_herd : herd_run_result)
    (locmap : (string, string) List.Assoc.t)
  : herd_analysis Or_error.t =
  let open Or_error.Let_syntax in
  (* The locmap function we supply below goes backwards, from
     assembly locations to C symbols.  Our location map goes the other
     way, so we need to transpose it. *)
  let locmap_r = List.Assoc.inverse locmap in
  match c_herd, a_herd with
  | `Disabled, _ | _, `Disabled -> return `Disabled
  | `Errored, _ -> return (`Errored `C)
  | _, `Errored -> return (`Errored `Assembly)
  | `Success initial, `Success final ->
    let%map outcome =
      Herd.outcome_of ~initial ~final
        ~locmap:(
          fun final_sym ->
            Or_error.return (
              List.Assoc.find ~equal:String.equal locmap_r final_sym
            )
        )
        (* TODO(@MattWindsor91): properly valmap, if needed. *)
        ~valmap:return
      in
      (outcome :> herd_analysis)
;;

(** [locations_of_herd_result r] takes the C/litmus Herd result [r]
   and extracts an associative array of mappings [(s, s')] where each
   [s] is a location mentioned in [r]'s state list, and each [s'] is
   the equivalent variable name in the memalloy C witness. *)
let locations_of_herd_result = function
  | `Success herd ->
    Herd.states herd
    |> List.concat_map ~f:Herd.State.bound
    |> List.map ~f:(fun s -> (s, lower_thread_local_symbol s))
  | `Disabled | `Errored -> []
;;

(** [compose_alists a b equal] produces an associative list that
    returns [(x, z)] for each [(x, y)] in [a] such that
    a [(y', z)] exists in [b] and [equal y y']. *)
let compose_alists
  (a : ('a, 'b) List.Assoc.t)
  (b : ('b, 'c) List.Assoc.t)
  (equal : 'b -> 'b -> bool)
  : ('a, 'c) List.Assoc.t =
  List.filter_map a ~f:(
    fun (k, v) ->
      Option.Monad_infix.(
        List.Assoc.find ~equal b v >>| Tuple2.create k
      )
  )
;;

let litmusify_single
    (o : OutputCtx.t) (fs : Pathset.File.t) locations cspec =
  (* The location symbols at the C level are each RHS of each
     pair in locs. *)
  let syms = List.map ~f:snd locations in
  let (id, spec) = Compiler.Full_spec.With_id.to_tuple cspec in
  let inp   = `File (Pathset.File.asm_path fs) in
  let outp  = `File (Pathset.File.lita_path fs) in
  log_stage "LITMUS" o (Pathset.File.basename fs) id;
  Common.litmusify o inp outp syms spec
;;

let run_single (o : OutputCtx.t) (ps: Pathset.t) herdprog cspec fname =
  let base = Filename.chop_extension (Filename.basename fname) in
  let open Or_error.Let_syntax in
  Pathset.File.(
    let start_time = Time.now () in

    (* NB: many of these stages depend on earlier stages' filesystem
       side-effects.  These dependencies aren't evident in the binding
       chain, so be careful when re-ordering. *)
    let fs = make ps base in
    let%bind c_herd =
      herd o herdprog cspec
        ~infile:(litc_path fs) ~outfile:(herdc_path fs)
    in
    let locs = locations_of_herd_result c_herd in
    let%bind () = compile o fs cspec in
    let%bind sym_redirects = litmusify_single o fs locs cspec in
    (* syms' now contains the redirections from C-level symbols to
       asm/Litmus-level symbols.  To get the mapping between Herd
       locations, we need the composition of the two maps. *)
    let locmap = compose_alists locs sym_redirects String.equal in
    let%bind a_herd =
      herd o herdprog cspec
        ~infile:(lita_path fs) ~outfile:(herda_path fs)
    in
    let%map analysis = analyse c_herd a_herd locmap in

    let end_time = Time.now () in

    ( base
    , { herd = analysis
      ; time_taken = Time.diff end_time start_time
      }
    )
  )
;;

let run_compiler (o : OutputCtx.t) ~in_root ~out_root herdprog c_fnames cspec
  : (Spec.Id.t * compiler_result) Or_error.t =
  let open Or_error.Let_syntax in
  let id = Compiler.Full_spec.With_id.id cspec in
  let%bind paths = Pathset.make_and_mkdirs id ~in_root ~out_root in
  Pathset.pp o.vf paths;
  Format.pp_print_newline o.vf ();

  let%map results =
    c_fnames
    |> List.sort ~compare:Core_extended.Extended_string.collate
    |> List.map ~f:(run_single o paths herdprog cspec)
    |> Or_error.combine_errors
  in
  (id, results)
;;

let check_c_files_exist c_path c_files =
    if List.is_empty c_files
    then Or_error.error_s
        [%message "Expected at least one C file." ~path:c_path]
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

let results_table_header =
  Staged.stage
    (List.map
       [ "Compiler"
       ; "File"
       ; "Result"
       ; "Time taken"
       ]
       ~f:(Fn.flip String.pp)
    )
;;

module Mapper = Fold_map.List.On_monad (Or_error)

let pp_herd_analysis f : herd_analysis -> unit = function
  | `Errored `Assembly -> String.pp f "ERROR (asm)"
  | `Errored `C        -> String.pp f "ERROR (C)"
  | `Disabled          -> String.pp f "--disabled--"
  | `Unknown           -> String.pp f "??"
  | `Undef             -> String.pp f "UNDEFINED BEHAVIOUR (asm)"
  | `OracleUndef       -> String.pp f "UNDEFINED BEHAVIOUR (C)"
  | `Equal             -> String.pp f "C == asm"
  | `Subset   _        -> String.pp f "C << asm"
  | `Superset _        -> String.pp f "C >> asm"
  | `NoOrder           -> String.pp f "C <> asm"
;;

let with_compiler_results tabulator compiler_id results =
  let rows =
    List.map results
      ~f:(fun (file, result) ->
          [ Fn.flip Spec.Id.pp compiler_id
          ; Fn.flip String.pp file
          ; Fn.flip pp_herd_analysis result.herd
          ; Fn.flip Time.Span.pp result.time_taken
          ])
  in
  Tabulator.with_rows rows tabulator
;;

let with_compiler_and_rules (print_rule, tabulator) (compiler_id, results) =
  let open Or_error.Let_syntax in
  let%bind t =
    (* Don't print a - rule directly below a '=' line *)
    if print_rule
    then Tabulator.with_rule '-' tabulator
    else return tabulator
  in
  let%map  t = with_compiler_results t compiler_id results in
  (true, t)
;;

let pp_results_table f results =
  let header = Staged.unstage results_table_header in
  Tabulator.(
    let open Or_error.Let_syntax in
    let%bind t = make ~header () >>= with_rule '=' in
    Mapper.foldM results ~init:(false, t) ~f:with_compiler_and_rules
    >>| snd >>| pp f
  )
;;

let pp_result f (r : full_result) =
  Format.pp_open_vbox f 0;
  let result = pp_results_table f r in
  Format.close_box ();
  Format.print_newline ();
  result
;;

let run ~in_root ~out_root o cfg =
  let open Or_error.Let_syntax in

  let specs = Config.M.compilers cfg in
  report_spec_errors o (List.filter_map ~f:snd (Config.M.disabled_compilers cfg));

  let herdprog = Config.M.herd_or_default cfg in

  let c_path = Filename.concat in_root "C" in
  let%bind c_files = Io.Dir.get_files c_path ~ext:"c" in
  let%bind () = check_c_files_exist c_path c_files in

  let results = Compiler.Full_spec.Set.map
      ~f:(run_compiler o ~in_root ~out_root herdprog c_files)
      specs
  in
  let%bind result = Or_error.combine_errors results in
  pp_result Format.std_formatter result
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
