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

let parse_c_asm (cn : string) (ps : Pathset.t) =
  R.reword_error
    (function
     | LangParser.Parse(perr) ->
        R.msg (MyFormat.format_to_string X86ATT.Frontend.pp_perr perr)
     | LangParser.Lex(lerr) ->
        R.msg (MyFormat.format_to_string X86ATT.Frontend.pp_lerr lerr)
    )
    (X86ATT.Frontend.run_file ~file:(asm_path_of cn ps))

module L = Litmus.T (X86ATT.Lang)
module S = Sanitiser.T (X86ATT.Lang) (Sanitiser.X86 (X86Dialect.ATTTraits))

let build_litmus (asm : X86ATT.Frontend.ast) =
  R.reword_error
    (fun err -> R.msg (MyFormat.format_to_string L.pp_err err))
    (L.make ~name:"TODO"
            ~init:[]
            ~programs:(S.sanitise asm.program))

let c_asm (env : env) (cn : string) (ps : Pathset.t) =
  parse_c_asm cn ps
  >>= build_litmus
  >>= (fun lit ->
    Format.fprintf env.vf "@[OUT@ %s@]@." ps.basename;
    Out_channel.with_file
      (lita_path_of cn ps)
      ~f:(fun oc ->
        let f = Format.formatter_of_out_channel oc in
        L.pp f lit;
        Format.pp_print_flush f ());
    Result.ok_unit)

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
                 >>= (fun _ -> Result.map ~f:ignore (c_asm env cn paths))

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

let command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Main driver for the Automagic Compiler Tormentor"
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
         and verbose_fmt =
           if verbose
           then Format.err_formatter
           else MyFormat.null_formatter ()
         in
         make_compiler_specs spec_file |>
           R.reword_error_msg (fun _ -> R.msg "Compiler specs are invalid.")
         >>= (
           fun specs ->
           pp_specs verbose_fmt specs;
           let env =
             { vf = verbose_fmt
             ; root = out_root_path
             ; specs = specs
             }
           in
           List.fold_result inpaths
                            ~init:()
                            ~f:(fun _ -> proc_results env specs verbose_fmt);
         )
         |>
           function
           | Ok _ -> ()
           | Error err ->
              Format.eprintf "@[Fatal error:@.@[%a@]@]@." R.pp_msg err
    ]

let () =
  Command.run command
