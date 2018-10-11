open Core
open Lib
open Utils
open Utils.MyContainers
open Lang

(* TODO: roll this into Pathset *)
let compiler_paths_of (cid : CompilerSpec.Id.t) (ps : Pathset.t) =
  List.Assoc.find_exn
    ps.compiler_paths
    cid
    ~equal:(List.equal ~equal:(=))

let asm_path_of (cid : CompilerSpec.Id.t) (ps : Pathset.t) : string =
  (compiler_paths_of cid ps).a_path

let lita_path_of (cid : CompilerSpec.Id.t) (ps : Pathset.t) : string =
  (compiler_paths_of cid ps).lita_path


module S = X86Specifics.Sanitiser (X86.ATT)
module E = Explainer.Make (X86.ATT)
module C = X86Conv.Make (X86.ATT) (X86.Herd7)
module L = X86Specifics.LitmusDirect


let output_litmus_oc (lit : L.t) (oc : Out_channel.t) =
  let f = Format.formatter_of_out_channel oc in
  L.pp f lit;
  Format.pp_print_flush f ()

let c_asm o (cid : CompilerSpec.Id.t) (spec : CompilerSpec.t) (ps : Pathset.t) =
  let open Io in
  let f src inp _ outp =
    let iname = MyFormat.format_to_string (In_source.pp) src in
    Litmusifier.run
      { o
      ; cid
      ; spec
      ; iname
      ; inp
      ; outp
      ; mode = `Litmusify
      }
  in
  with_input_and_output
    (`File (asm_path_of cid ps))
    (`File (lita_path_of cid ps))
    ~f

let proc_c (o : OutputCtx.t) root specs results_path c_fname =
  let open Or_error in
  let paths = Pathset.make specs
                           ~root_path:root
                           ~results_path:results_path
                           ~c_fname:c_fname in
  Pathset.pp o.vf paths;
  Format.pp_print_newline o.vf ();

  tag (Pathset.make_dir_structure paths)
    ~tag:"couldn't make dir structure"
  *>
    MyList.iter_result
      (fun (cid, cs) ->
         Format.fprintf o.vf "@[CC[%a]@ %s@]@."
           CompilerSpec.Id.pp cid
           paths.basename;
         Compiler.compile cid cs paths
         *> c_asm o cid cs paths
      ) specs

let do_memalloy (o : OutputCtx.t) root specs (results_path : string) =
  let c_path = Filename.concat results_path "C" in
  Or_error.try_with_join
    (
      fun () ->
        Sys.readdir c_path
        |> Array.filter ~f:(MyFilename.has_extension ~ext:"c")
        |> MyArray.iter_result (proc_c o root specs results_path)
    )

let pp_specs (f : Format.formatter) (specs : CompilerSpec.set) : unit =
  Format.pp_open_vbox f 0;
  Format.pp_print_string f "Compiler specs --";
  Format.pp_print_break f 0 4;
  Format.pp_open_vbox f 0;
  List.iter ~f:(fun (c, s) ->
      MyFormat.pp_kv f (CompilerSpec.Id.to_string c) CompilerSpec.pp s) specs;
  Format.pp_close_box f ();
  Format.pp_print_cut f ();
  Format.pp_close_box f ();
  Format.pp_print_flush f ()

let maybe_err_formatter on =
  if on
  then Format.err_formatter
  else MyFormat.null_formatter ()

let prerr =
  function
  | Ok _ -> ()
  | Error err ->
     Format.eprintf "@[Fatal error:@.@[%a@]@]@." Error.pp err

let get_spec specs compiler_id =
  List.Assoc.find specs ~equal:(CompilerSpec.Id.equal) compiler_id
  |> Result.of_option
    ~error:(Error.create "invalid compiler ID" compiler_id [%sexp_of: CompilerSpec.Id.t])

let do_litmusify mode o infile outfile (cid : CompilerSpec.Id.t) specs =
  let open Result.Let_syntax in
  let%bind spec = get_spec specs cid in
  Io.(
    let f src inp _ outp =
      let iname = MyFormat.format_to_string (In_source.pp) src in
      Litmusifier.run
        { o
        ; cid
        ; spec
        ; iname
        ; inp
        ; outp
        ; mode
        }
    in
    with_input_and_output
      (In_source.of_option infile)
      (Out_sink.of_option outfile)
      ~f
  )

let output_explanation_oc (expl : E.t) (oc : Out_channel.t) =
  let f = Format.formatter_of_out_channel oc in
  E.pp f expl;
  Format.pp_print_flush f ()

let output_explanation (expl : E.t) (file : string option) =
  match file with
  | Some file -> Out_channel.with_file file ~f:(output_explanation_oc expl)
  | None -> output_explanation_oc expl Out_channel.stdout

let explain =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Explains act's understanding of an assembly file"
    [%map_open
     let spec_file =
       flag "spec"
            (optional_with_default
               (Filename.concat Filename.current_dir_name "compiler.spec")
               string)
            ~doc: "PATH the compiler spec file to use"
     and verbose =
       flag "verbose"
            no_arg
            ~doc: "verbose mode"
     and no_warnings =
        flag "no-warnings"
          no_arg
          ~doc: "silence all warnings"
     and compiler_id =
       anon ("COMPILER_ID" %: string)
     and outfile =
       flag "output"
            (optional string)
            ~doc: "FILE the explanation output file (default: stdout)"
     and infile =
       anon (maybe ("FILE" %: string))
     in
     fun () ->
       let warnings = not no_warnings in
       let cid = CompilerSpec.Id.of_string compiler_id in
       let o = OutputCtx.make ~verbose ~warnings in
       Or_error.Let_syntax.(
         let%bind specs = Compiler.load_and_test_specs ~path:spec_file in
         pp_specs o.vf specs;
         do_litmusify `Explain o infile outfile cid specs
       )
       |> prerr
    ]

let run_herd prog litname _ oc =
  Run.run ~oc ~prog [litname]

let litmusify =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Converts an assembly file to a litmus test"
    [%map_open
     let spec_file =
       flag "spec"
            (optional_with_default
               (Filename.concat Filename.current_dir_name "compiler.spec")
               string)
            ~doc: "PATH the compiler spec file to use"
     and verbose =
       flag "verbose"
         no_arg
         ~doc: "verbose mode"
     and no_warnings =
       flag "no-warnings"
         no_arg
         ~doc: "silence all warnings"
     and sendto =
       flag "sendto"
         (optional string)
         ~doc: "CMDNAME pass generated litmus through this Herd-like command"
     and compiler_id =
       anon ("COMPILER_ID" %: string)
     and outfile =
       flag "output"
            (optional string)
            ~doc: "FILE the litmus output file (default: stdout)"
     and infile =
       anon (maybe ("FILE" %: string))
     in
     fun () ->
       let warnings = not no_warnings in
       let cid = CompilerSpec.Id.of_string compiler_id in
       let o = OutputCtx.make ~verbose ~warnings in
       Result.Let_syntax.(
         let%bind specs = Compiler.load_and_test_specs ~path:spec_file in
         pp_specs o.vf specs;
         match sendto with
         | None -> do_litmusify `Litmusify o infile outfile cid specs
         | Some cmd ->
           let tmpname = Filename.temp_file "act" "litmus" in
           let cid = CompilerSpec.Id.of_string compiler_id in
           let%bind _ = do_litmusify `Litmusify o infile (Some tmpname) cid specs in
           Io.Out_sink.with_output ~f:(run_herd cmd tmpname)
             (Io.Out_sink.of_option outfile)
       )
       |> prerr
    ]

let memalloy =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Runs automatic testing over a memalloy output directory"
    [%map_open
      let spec_file =
        flag "spec"
          (optional_with_default
             (Filename.concat Filename.current_dir_name "compiler.spec")
             string)
          ~doc: "PATH the compiler spec file to use"
      and root =
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
      and inpath =
        anon ("RESULTS_PATH" %: string)
      in
      fun () ->
        let warnings = not no_warnings in
        let o = OutputCtx.make ~verbose ~warnings in
        Result.Let_syntax.(
          let%bind specs = Compiler.load_and_test_specs ~path:spec_file in
          pp_specs o.vf specs;
          do_memalloy o root specs inpath
        ) |> prerr
]

let command =
  Command.group
    ~summary:"Automagic Compiler Tormentor"
    [ "explain"  , explain
    ; "litmusify", litmusify
    ; "memalloy" , memalloy
    ]

let () = Command.run command
