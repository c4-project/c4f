open Core
open Rresult
open Lib
open AsmParsers

type config =
  {
    verbose : bool ref;

    out_root_path : string ref;

    results_paths : string Queue.t;

    spec_file : string ref;
  }

let iter_result (f : 'a -> (unit, 'e) result)
    : 'a list -> (unit, 'e) result =
  List.fold_result ~init:() ~f:(fun _ -> f)


let usage = "act [paths to comparator output]"

let c_path_of results_path : string -> string =
  Filename.concat (Filename.concat results_path "C")
let litc_path_of results_path : string -> string =
  Filename.concat (Filename.concat results_path "litmus")

let a_dir_of (root : string) (cname : string) : string =
  Filename.concat root (cname ^ "_asm")

let a_path_of (root : string) (file : string) (cname : string) : string =
  Filename.concat (a_dir_of root cname) file

let lita_dir_of (root : string) (cname : string) : string =
  Filename.concat root (cname ^ "_litmus")

let lita_path_of (root : string) (file : string) (cname : string) : string =
  Filename.concat (lita_dir_of root cname) file

type pathset =
  { c_path     : string                 (* Path to the executable C file     *)
  ; litc_path  : string                 (* Path to the C litmus test         *)
  ; out_root   : string                 (* Root for the output dir structure *)
  ; a_paths    : (string * string) list (* Paths to output for each compiler *)
  ; lita_paths : (string * string) list (* Paths to litmus for each compiler *)
  }

let gen_pathset
      (specs : CompilerSpec.set)
      (root_path : string)
      (results_path : string)
      (c_fname : string)
    : pathset =
  let basename   = Filename.basename (Filename.chop_extension c_fname) in
  let lit_fname  = basename ^ ".litmus" in
  let spec_map f = List.map ~f:(fun (c, _) -> (c, f c)) specs in
  let asm_fname  = basename ^ ".s" in
  { out_root     = root_path
  ; c_path       = c_path_of    results_path c_fname
  ; litc_path    = litc_path_of results_path lit_fname
  ; a_paths      = spec_map (a_path_of root_path asm_fname)
  ; lita_paths   = spec_map (lita_path_of root_path lit_fname)
  }

type ent_type =
  | File
  | Dir
  | Nothing
  | Unknown

let get_ent_type (path : string) : ent_type =
  match Sys.file_exists path with
  | `No -> Nothing
  | `Unknown -> Unknown
  | `Yes ->
     match Sys.is_directory path with
     | `No -> File
     | `Unknown -> Unknown
     | `Yes -> Dir

(** [mkdir path] tries to make a directory at path [path].
    If [path] exists and is a directory, it does nothing.
    If [path] exists but is a file, or another error occurred, it returns
    an error message. *)
let mkdir (path : string) =
  match get_ent_type path with
  | Dir -> R.ok ()
  | File -> R.error_msgf "%s exists, but is a file" path
  | Unknown -> R.error_msgf "couldn't determine whether %s already exists" path
  | Nothing ->
     try
       Unix.mkdir path;
       R.ok ()
     with
     | Unix.Unix_error (errno, func, arg) ->
        R.error_msgf "couldn't %s %s: %s"
                     func arg (Unix.Error.message errno)

let make_dir_structure (ps : pathset) =
  try
    if Sys.is_directory_exn ps.out_root
    then iter_result mkdir
                     (List.map ~f:(fun (c, _) -> a_dir_of ps.out_root c) ps.a_paths
                      @ List.map ~f:(fun (c, _) -> lita_dir_of ps.out_root c) ps.lita_paths)
    else R.error_msgf "%s not a directory" ps.out_root
  with
  | Sys_error e -> R.error_msgf "system error while making directories: %s" e

let run_cc (cc : string) (argv : string list) =
  let proc = Unix.create_process ~prog:cc
                                 ~args:argv
  in
  let errch = Unix.in_channel_of_descr proc.stderr in
  let res =
    Unix.waitpid proc.pid
    |> R.reword_error
         (function
          | `Exit_non_zero ret ->
             let outp = Stdio.In_channel.input_lines errch in
             R.msgf "'%s' exited with code %d with error:\n%s"
                    (String.concat ~sep:" " (cc::argv))
                    ret
                    (String.concat ~sep:"\n" outp)
          | `Signal sg ->
             R.msgf "'%s' caught signal %s"
                    (String.concat ~sep:" " (cc::argv))
                    (Signal.to_string sg))
  in
  Stdio.In_channel.close errch;
  res

let asm_path_of (cc_id : string) (ps : pathset) : string =
  List.Assoc.find_exn ps.a_paths cc_id ~equal:(=)

module XL = X86Lexer.Make(LexUtils.Default)

let print_position oc (pos : Lexing.position) =
  fprintf oc "%s:%d:%d"
          pos.pos_fname
          pos.pos_lnum
          (pos.pos_cnum - pos.pos_bol)

let parse_x86_ic (asm_fname : string) (asm_ic : In_channel.t) : unit =
  let lexbuf = Lexing.from_channel asm_ic in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = asm_fname };
  try
    while true do
      let _ = X86Parser.main XL.token lexbuf in
      ()
    done
  with
  | LexMisc.Error (e, _) ->
     eprintf "lexing error: %s (in %a)\n"
             e
             print_position lexbuf.lex_curr_p
  | X86Base.ParseError ((s, e), ty) ->
     eprintf "parsing error in %s at %a \n"
             (X86Base.print_parse_error ty)
             Pos.pp_pos2 (s, e)
  | X86Parser.Error ->
     eprintf "parsing error (in %a)\n"
             print_position lexbuf.lex_curr_p

let parse_x86_file (asm_fname : string) =
  In_channel.with_file asm_fname ~f:(parse_x86_ic asm_fname)

let compile (cc_id : string) (cc_spec : CompilerSpec.t) (ps : pathset) =
  let asm_path = List.Assoc.find_exn ps.a_paths cc_id ~equal:(=) in
  let final_argv =
  [ "-S"       (* emit assembly *)
  ; "-fno-pic" (* don't emit position-independent code *)
  ]
  @ cc_spec.argv
  @ ["-o"
    ; asm_path
    ; ps.c_path
    ] in
  run_cc cc_spec.cmd final_argv

let summarise_pathset (vf : Format.formatter) (ps : pathset) : unit =
  List.iter
    ~f:(fun (io, t, v) -> Format.fprintf vf "@[[%s] %s file:@ %s@]@." io t v)
    ( [ ("in" , "C"         , ps.c_path)
      ; ("in" , "C Litmus"  , ps.litc_path)
      ]
      @ List.map ~f:(fun (c, p) -> ("out", c, p)) ps.a_paths
      @ List.map ~f:(fun (c, p) -> ("out", c ^ " (litmus)", p)) ps.lita_paths)

let proc_c (cfg : config) (cc_specs : CompilerSpec.set) vf results_path c_fname =
  let root = !(cfg.out_root_path) in
  let paths = gen_pathset cc_specs root results_path c_fname in
  summarise_pathset vf paths;
  make_dir_structure paths |>
    R.reword_error_msg (fun _ -> R.msg "couldn't make dir structure")
  >>= (
    fun _ -> iter_result
               (fun (cn, cs) ->
                 let r = compile cn cs paths in
                 (* Temporary *)
                 parse_x86_file (asm_path_of cn paths);
                 r
               ) cc_specs
  )

let proc_results (cfg : config) (cc_specs : CompilerSpec.set) (vf : Format.formatter) (results_path : string) =
  let c_path = Filename.concat results_path "C" in
  try
    Sys.readdir c_path
    |> Array.filter ~f:(fun file -> snd (Filename.split_extension file) = Some "c")
    |> Array.fold_result
         ~init:()
         ~f:(fun _ -> proc_c cfg cc_specs vf results_path)
  with
    Sys_error e -> R.error_msgf "system error: %s" e

let summarise_config (cfg : config) (f : Format.formatter) : unit =
  Format.pp_open_vbox f 0;
  Format.pp_print_string f "Config --";
  Format.pp_print_break f 0 4;
  Format.pp_open_vbox f 0;
  MyFormat.pp_kv f "Reading compiler specs from" MyFormat.pp_sr cfg.spec_file;
  Format.pp_print_cut f ();
  MyFormat.pp_kv f "memalloy results paths" MyFormat.pp_sq cfg.results_paths;
  Format.pp_close_box f ();
  Format.pp_print_cut f ();
  Format.pp_close_box f ();
  Format.pp_print_flush f ()

let summarise_specs (f : Format.formatter) (specs : CompilerSpec.set) : unit =
  Format.pp_open_vbox f 0;
  Format.pp_print_string f "Compiler specs --";
  Format.pp_print_break f 0 4;
  Format.pp_open_vbox f 0;
  List.iter ~f:(fun (c, s) -> MyFormat.pp_kv f c CompilerSpec.pp s) specs;
  Format.pp_close_box f ();
  Format.pp_print_cut f ();
  Format.pp_close_box f ();
  Format.pp_print_flush f ()

let null_formatter () : Format.formatter =
  Format.make_formatter (fun _ _ _ -> ()) (fun _ -> ())

let test_compiler (cc : string) =
  run_cc cc ["--version"]

(** [make_compiler_specs specpath] reads in the compiler spec list at
    [specpath], converting it to a [compiler_spec_set]. *)

let make_compiler_specs (specpath : string) =
  CompilerSpec.load_specs ~path:specpath
  |> List.fold_result ~init:[]
                      ~f:(fun specs (c, spec) ->
                        test_compiler (spec.cmd) >>| (fun _ -> (c, spec)::specs)
                      )
let () =
  let cfg : config =
    { verbose = ref false
    ; results_paths = Queue.create ()
    ; out_root_path = ref Filename.current_dir_name
    ; spec_file = ref (Filename.concat Filename.current_dir_name "compiler.spec")
    }
  in

  let spec =
    [ ("-o", Arg.Set_string cfg.out_root_path,
       "The path under which output directories will be created.")
    ; ("-v", Arg.Set cfg.verbose,
       "Verbose mode.")
    ; ("-c", Arg.Set_string cfg.spec_file,
       "The compiler spec file to use.")
    ; ("--", Arg.Rest (Queue.enqueue cfg.results_paths), "")
    ]
  in
  Arg.parse spec (Queue.enqueue cfg.results_paths) usage;

  let verbose_fmt =
    if !(cfg.verbose)
    then Format.err_formatter
    else null_formatter ()
  in
  summarise_config cfg verbose_fmt;

  make_compiler_specs !(cfg.spec_file) |>
    R.reword_error_msg (fun _ -> R.msg "Compiler specs are invalid.")
  >>= (
    fun specs ->
    summarise_specs verbose_fmt specs;
    Queue.fold_result cfg.results_paths
                      ~init:()
                      ~f:(fun _ -> proc_results cfg specs verbose_fmt);
  )
  |>
    function
    | Ok _ -> ()
    | Error err ->
       Format.eprintf "@[Fatal error:@.@[%a@]@]@." R.pp_msg err
