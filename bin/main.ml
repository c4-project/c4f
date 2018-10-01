open Core
open Rresult
open Lib
open Utils
open Utils.MyContainers
open Lang

type env =
  { vf    : Format.formatter
  ; root  : string
  ; specs : CompilerSpec.set
  }

let asm_path_of (cc_id : string) (ps : Pathset.t) : string =
  List.Assoc.find_exn ps.a_paths cc_id ~equal:(=)

let lita_path_of (cc_id : string) (ps : Pathset.t) : string =
  List.Assoc.find_exn ps.lita_paths cc_id ~equal:(=)

let parse_asm (cs : CompilerSpec.t) (file : string option) =
  match cs.emits with
  | Language.X86 (X86Dialect.Att) ->
     R.reword_error
       (function
        | LangParser.Parse(perr) ->
           R.msg (MyFormat.format_to_string X86ATT.Frontend.pp_perr perr)
        | LangParser.Lex(lerr) ->
           R.msg (MyFormat.format_to_string X86ATT.Frontend.pp_lerr lerr)
       )
       (match file with
        | Some file -> X86ATT.Frontend.run_file ~file
        | None      -> X86ATT.Frontend.run_stdin ())
  | Language.X86 d ->
     R.error_msgf "FIXME: unsupported x86 dialect %s"
                  (X86Dialect.Map.to_string d |> Option.value ~default:"(unknown)")

module L = Litmus.T (X86ATT.Lang)
module S = Sanitiser.T (X86ATT.Lang) (Sanitiser.X86 (X86Dialect.ATTTraits))

let build_litmus (asm : X86ATT.Frontend.ast) =
  R.reword_error
    (fun err -> R.msg (MyFormat.format_to_string L.pp_err err))
    (L.make ~name:"TODO"
            ~init:[]
            ~programs:(S.sanitise asm.program))

let output_litmus_oc (lit : L.t) (oc : Out_channel.t) =
  let f = Format.formatter_of_out_channel oc in
  L.pp f lit;
  Format.pp_print_flush f ()

let output_litmus (lit : L.t) (file : string option) =
  match file with
  | Some file -> Out_channel.with_file file ~f:(output_litmus_oc lit)
  | None -> output_litmus_oc lit Out_channel.stdout

let c_asm (env : env) (cn : string) (cs : CompilerSpec.t) (ps : Pathset.t) =
  let open Result.Let_syntax in
  let%bind asm = parse_asm cs (Some (asm_path_of cn ps)) in
  let%bind lit = build_litmus asm in
  Format.fprintf env.vf "@[OUT@ %s@]@." ps.basename;
  output_litmus lit (Some (lita_path_of cn ps));
  (* TODO(@MattWindsor91): catch errors from output_litmus. *)
  Result.ok_unit


let proc_c (env : env) (cc_specs : CompilerSpec.set) vf results_path c_fname =
  let paths = Pathset.make cc_specs
                           ~root_path:env.root
                           ~results_path:results_path
                           ~c_fname:c_fname in
  Pathset.pp vf paths;
  Format.pp_print_newline vf ();

  Pathset.make_dir_structure paths |>
    R.reword_error_msg (fun _ -> R.msg "couldn't make dir structure")
  >>= (
    fun _ -> MyList.iter_result
               (fun (cn, cs) ->
                 Format.fprintf vf "@[CC[%s]@ %s@]@." cn paths.basename;
                 Compiler.compile cn cs paths
                 >>= (fun _ -> Result.map ~f:ignore (c_asm env cn cs paths))

               ) cc_specs
  )

let proc_results (env : env) (cc_specs : CompilerSpec.set) (vf : Format.formatter) (results_path : string) =
  let c_path = Filename.concat results_path "C" in
  try
    Sys.readdir c_path
    |> Array.filter ~f:(MyFilename.has_extension ~ext:"c")
    |> MyArray.iter_result (proc_c env cc_specs vf results_path)
  with
    Sys_error e -> R.error_msgf "system error: %s" e

let pp_specs (f : Format.formatter) (specs : CompilerSpec.set) : unit =
  Format.pp_open_vbox f 0;
  Format.pp_print_string f "Compiler specs --";
  Format.pp_print_break f 0 4;
  Format.pp_open_vbox f 0;
  List.iter ~f:(fun (c, s) -> MyFormat.pp_kv f c CompilerSpec.pp s) specs;
  Format.pp_close_box f ();
  Format.pp_print_cut f ();
  Format.pp_close_box f ();
  Format.pp_print_flush f ()

(** [make_compiler_specs specpath] reads in the compiler spec list at
    [specpath], converting it to a [compiler_spec_set]. *)
let make_compiler_specs (specpath : string) =
  CompilerSpec.load_specs ~path:specpath
  |> List.fold_result ~init:[]
                      ~f:(fun specs (c, spec) ->
                        Compiler.test spec >>| (fun _ -> (c, spec)::specs)
                      )
  |> R.reword_error_msg (fun _ -> R.msg "Compiler specs are invalid.")

let verbose_formatter verbose =
  if verbose
  then Format.err_formatter
  else MyFormat.null_formatter ()

let prerr =
  function
  | Ok _ -> ()
  | Error err ->
     Format.eprintf "@[Fatal error:@.@[%a@]@]@." R.pp_msg err

let get_spec specs compiler_id =
  List.Assoc.find specs ~equal:(=) compiler_id
  |> Result.of_option ~error:(R.msgf "invalid compiler ID: %s" compiler_id)

let do_litmusify _ infile outfile compiler_id specs =
  let open Result.Let_syntax in
  let%bind spec = get_spec specs compiler_id in
  let%bind asm = parse_asm spec infile in
  let%bind lit = build_litmus asm in
  output_litmus lit outfile;
  Result.ok_unit

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
         let vf = verbose_formatter verbose in
         make_compiler_specs spec_file
         >>= do_litmusify vf infile outfile compiler_id |> prerr
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
     and out_root_path =
       flag "output"
            (optional_with_default
               Filename.current_dir_name
               string)
            ~doc: "PATH the path under which output directories will be created"
     and verbose =
       flag "verbose"
            no_arg
            ~doc: "verbose mode"
     and inpaths_anon =
       anon (maybe (non_empty_sequence_as_list ("PATH" %: string)))
     and inpaths_rest =
       flag "--"
            escape
            ~doc: "PATHS any remaining arguments are treated as input paths"
         in
         fun () ->
         let inpaths =
           (Option.value ~default:[] inpaths_anon)
           @ (Option.value ~default:[] inpaths_rest)
         and vf = verbose_formatter verbose in
         make_compiler_specs spec_file
         >>= (
           fun specs ->
           pp_specs vf specs;
           let env =
             { vf
             ; root = out_root_path
             ; specs
             }
           in
           List.fold_result inpaths
                            ~init:()
                            ~f:(fun _ -> proc_results env specs vf);
         )
         |> prerr
    ]

let command =
  Command.group
    ~summary:"Automagic Compiler Tormentor"
    [ "memalloy", memalloy
    ; "litmusify", litmusify
    ]

let () = Command.run command
