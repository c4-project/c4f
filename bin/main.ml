open Core
open Lib
open Utils
open Utils.MyContainers
open Lang

type env =
  { vf    : Format.formatter
  ; wf    : Format.formatter
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
    let fname = Option.value ~default:"(stdin)" file in
    Or_error.tag_arg
      ( match file with
        | Some file -> X86.AttFrontend.run_file ~file
        | None      -> X86.AttFrontend.run_stdin ()
      )
      "assembly parse error" fname String.sexp_of_t
  | Language.X86 _ ->
    Or_error.error "language unsupported" cs.emits Language.sexp_of_name

module S = X86Specifics.Sanitiser (X86.ATT)
module E = Explainer.Make (X86.ATT)
module C = X86Conv.Make (X86.ATT) (X86.Herd7)
module L = X86Specifics.LitmusDirect

let emit_warnings wf name =
  function
  | [] -> ()
  | ws ->
    let pp_warning f w =
      Format.fprintf f "@[<h>-@ @[<hov>%a@]@]@,"
        S.Warn.pp w
    in
    let pp_name f s =
      Format.fprintf f "@ for@ %s" s
    in
    Format.fprintf wf "Warnings%a:@ @[<v>%a@]@."
      (MyFormat.pp_option ~pp:pp_name) name
      (fun f -> List.iter ~f:(pp_warning f)) ws

let build_litmus (wf : Format.formatter) (name : string option) (asm : X86.AttFrontend.ast) =
  let {S.programs; warnings} = S.sanitise asm.program in
  emit_warnings wf name warnings;
  Or_error.tag
    ~tag:"couldn't build litmus file"
    ( L.make ~name:(Option.value name ~default:"anonymous")
             ~init:[]
             ~programs:(List.map ~f:C.convert programs)
    )


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
  let%bind lit = build_litmus env.wf (Some ps.basename) asm in
  Format.fprintf env.vf "@[OUT@ %s@]@." ps.basename;
  output_litmus lit (Some (lita_path_of cn ps));
  (* TODO(@MattWindsor91): catch errors from output_litmus. *)
  Result.ok_unit


let proc_c (env : env) results_path c_fname =
  let open Or_error in
  let paths = Pathset.make env.specs
                           ~root_path:env.root
                           ~results_path:results_path
                           ~c_fname:c_fname in
  Pathset.pp env.vf paths;
  Format.pp_print_newline env.vf ();

  tag (Pathset.make_dir_structure paths)
    ~tag:"couldn't make dir structure"
  *>
    MyList.iter_result
      (fun (cn, cs) ->
         Format.fprintf env.vf "@[CC[%s]@ %s@]@." cn paths.basename;
         Compiler.compile cn cs paths
         *> c_asm env cn cs paths
      ) env.specs

let proc_results (env : env) (results_path : string) =
  let c_path = Filename.concat results_path "C" in
  Or_error.try_with_join
    (
      fun () ->
        Sys.readdir c_path
        |> Array.filter ~f:(MyFilename.has_extension ~ext:"c")
        |> MyArray.iter_result (proc_c env results_path)
    )

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

(** [tap f r] behaves like [Result.iter f r], but returns [r]. *)
let tap (f : 'a -> unit) (r : ('a, 'e) result) : ('a, 'e) result =
  Result.iter ~f r; r

(** [make_compiler_specs vf specpath] reads in the compiler spec list at
    [specpath], converting it to a [compiler_spec_set].
    It pretty-prints the specs onto [vf]. *)
let make_compiler_specs (vf : Format.formatter) (specpath : string) =
  let open Or_error in
  CompilerSpec.load_specs ~path:specpath
  |> List.fold_result ~init:[]
                      ~f:(fun specs (c, spec) ->
                        Compiler.test spec >>| (fun _ -> (c, spec)::specs)
                      )
  |> tap (pp_specs vf)
  |> tag ~tag:"compiler specs are invalid"

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
  List.Assoc.find specs ~equal:(=) compiler_id
  |> Result.of_option
    ~error:(Error.create "invalid compiler ID" compiler_id [%sexp_of: string])

let do_litmusify _ wf infile outfile compiler_id specs =
  let open Result.Let_syntax in
  let%bind spec = get_spec specs compiler_id in
  let%bind asm = parse_asm spec infile in
  let%bind lit = build_litmus wf Option.(infile >>| Filename.basename) asm in
  output_litmus lit outfile;
  Result.ok_unit

let output_explanation_oc (expl : E.t) (oc : Out_channel.t) =
  let f = Format.formatter_of_out_channel oc in
  E.pp f expl;
  Format.pp_print_flush f ()

let output_explanation (expl : E.t) (file : string option) =
  match file with
  | Some file -> Out_channel.with_file file ~f:(output_explanation_oc expl)
  | None -> output_explanation_oc expl Out_channel.stdout

let do_explain _ infile outfile compiler_id specs =
  let open Result.Let_syntax in
  let%bind spec = get_spec specs compiler_id in
  let%bind asm = parse_asm spec infile in
  output_explanation (E.explain asm.program) outfile;
  Result.ok_unit

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
         let vf = maybe_err_formatter verbose in
         Result.Let_syntax.(
           make_compiler_specs vf spec_file
           >>= do_explain vf infile outfile compiler_id
         )
         |> prerr
    ]

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
         let vf = maybe_err_formatter verbose in
         let wf = maybe_err_formatter (not no_warnings) in
         Result.Let_syntax.(
           let%bind specs = make_compiler_specs vf spec_file in
           do_litmusify vf wf infile outfile compiler_id specs
         )
         |> prerr
    ]

let do_memalloy env inpaths =
  List.fold_result inpaths
                   ~init:()
                   ~f:(fun _ -> proc_results env)

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
        in
        let vf = maybe_err_formatter verbose in
        let wf = maybe_err_formatter (not no_warnings) in
        Result.Let_syntax.(
         let%bind specs = make_compiler_specs vf spec_file in
         do_memalloy { vf; wf; root; specs } inpaths
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
