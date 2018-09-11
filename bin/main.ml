open Rresult

type config =
  {
    verbose : bool ref;

    out_root_path : string ref;

    results_paths : string Queue.t;

    (* The x86 compiler to use. *)
    x86_cc : string ref;

    (* Arguments to the x86 compiler. *)
    x86_argv : string Queue.t;
  }

(** [compiler_spec] describes how to invoke a compiler. *)
type compiler_spec =
  {
    cc : string;
    argv: string list;
  }

let usage = "act [paths to comparator output]"

let c_path_of results_path = Filename.concat results_path "C"
let litc_path_of results_path = Filename.concat results_path "litmus"

type pathset =
  {
    (* Path to the executable C file. *)
    c_path    : string;
    (* Path to the C litmus test. *)
    litc_path : string;
  }

let gen_pathset results_path c_fname =
  let basename = Filename.basename (Filename.remove_extension c_fname) in
  let c_fpath = Filename.concat (c_path_of results_path) c_fname in
  let lit_fname = basename ^ ".litmus" in
  let lit_fpath = Filename.concat (litc_path_of results_path) lit_fname in
  { c_path = c_fpath; litc_path = lit_fpath }

let proc_c_err vf results_path c_fname =
  let paths = gen_pathset results_path c_fname in
  Format.fprintf vf "@[C file:@ %s@]@.Litmus file:@ %s@]@." paths.c_path paths.litc_path;
  R.ok ()

let proc_c vf results_path c_fname : unit =
  match proc_c_err vf results_path c_fname with
  | Ok _ -> ()
  | Error err ->
     Format.eprintf "@[error:@ %a@]@." R.pp_msg err

let proc_results (vf : Format.formatter) (_ : config) (results_path : string) : unit =
  let c_path = Filename.concat results_path "C" in
  try
    let c_files = Sys.readdir c_path in
    Array.iter (proc_c vf results_path) c_files
  with
    Sys_error e ->
     Format.eprintf "@[system error:@ %s@]@." e

let qpush (q : string Queue.t) (s : string) : unit =
  Queue.push s q

let pp_kv (k : string) (v : Format.formatter -> unit) (f : Format.formatter) : unit =
  Format.pp_open_hvbox f 0;
  Format.pp_print_string f k;
  Format.pp_print_string f ":";
  Format.pp_print_break f 2 1;
  v f;
  Format.pp_close_box f ();
  Format.pp_print_cut f ()

let pp_sr (s : string ref) (f : Format.formatter) : unit =
  Format.pp_print_string f !s

let pp_sq (sq : string Queue.t) (f : Format.formatter) : unit =
  let first = ref true in
  Queue.iter (fun s -> begin
                  if not !first then Format.pp_print_space f ();
                  first := false;
                  Format.pp_print_string f s
                end)
             sq

let summarise_config (cfg : config) (f : Format.formatter) : unit =
  Format.pp_open_vbox f 0;
  Format.pp_print_string f "Config --";
  Format.pp_print_break f 0 4;
  Format.pp_open_vbox f 0;
  pp_kv "C compiler (x86)" (pp_sr cfg.x86_cc) f;
  pp_kv "C compiler (x86) args" (pp_sq cfg.x86_argv) f;
  pp_kv "memalloy results paths" (pp_sq cfg.results_paths) f;
  Format.pp_close_box f ();
  Format.pp_close_box f ()

let null_formatter () : Format.formatter =
  Format.make_formatter (fun _ _ _ -> ()) (fun _ -> ())

let test_compiler (cc : string) =
  let pid = Unix.create_process cc
                                [|cc; "--version"|]
                                Unix.stdin
                                Unix.stdout
                                Unix.stderr
  in
  let (_, stat) = Unix.waitpid [] pid in
  match stat with
  | Unix.WEXITED ret when ret = 0 -> R.ok ()
  | Unix.WEXITED ret -> R.error_msgf "exited with code %d" ret
  | Unix.WSIGNALED sg -> R.error_msgf "compiler killed by signal %d" sg
  | Unix.WSTOPPED sg -> R.error_msgf "compiler stopped by signal %d" sg

let default_compiler_argv : string list =
  [ "-S"       (* emit assembly *)
  ; "-no-fpic" (* don't emit position-independent code *)
  ]

let make_compiler_argv (argv : string Queue.t) : string list =
  match List.of_seq (Queue.to_seq argv) with
  | [] -> ["-S"; "-no-fpic"]
  | xs -> xs

let make_compiler_spec (cc : string) (argv : string Queue.t) =
  test_compiler cc
  |> R.reword_error_msg (fun _ -> R.msg "Compiler test failed")
  |> R.map (fun _ -> { cc = cc
                     ; argv = make_compiler_argv argv
                     }
           )

let () =
  let cfg : config =
    { verbose = ref false
    ; results_paths = Queue.create ()
    ; out_root_path = ref Filename.current_dir_name
    ; x86_cc = ref "gcc"
    ; x86_argv = Queue.create ()
    }
  in

  let spec =
    [ ("-o", Arg.Set_string cfg.out_root_path,
       "The path under which output directories will be created.")
    ; ("-v", Arg.Set cfg.verbose,
       "Verbose mode.")
    ; ("-xc", Arg.Set_string cfg.x86_cc,
       "The x86 compiler to use.")
    ; ("-xa", Arg.String (qpush cfg.x86_argv),
       "Adds an argument to the x86 compiler.")
    ; ("--", Arg.Rest (qpush cfg.results_paths), "")
    ]
  in
  Arg.parse spec (qpush cfg.results_paths) usage;

  let verbose_fmt =
    if !(cfg.verbose)
    then Format.err_formatter
    else null_formatter ()
  in
  summarise_config cfg verbose_fmt;

  let x86_spec = make_compiler_spec !(cfg.x86_cc) cfg.x86_argv in
  begin
    match x86_spec with
    | Ok _ -> ()
    | Error err ->
       Format.eprintf "@[invalid x86 compiler:@ %a@]@." R.pp_msg err
  end;

  Queue.iter (proc_results verbose_fmt cfg) cfg.results_paths
