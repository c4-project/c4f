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
  { cc   : string
  ; argv : string list
  }

let usage = "act [paths to comparator output]"

let c_path_of results_path = Filename.concat results_path "C"
let litc_path_of results_path = Filename.concat results_path "litmus"
let x86_path_of root = Filename.concat root "x86_asm"
let litx86_path_of root = Filename.concat root "x86_litmus"

type pathset =
  { c_path      : string (* Path to the executable C file. *)
  ; litc_path   : string (* Path to the C litmus test. *)
  ; x86_path    : string (* Path to the compiled x86 output. *)
  ; litx86_path : string (* Path to the x86 litmus test. *)
  }

let gen_pathset (cfg : config) results_path c_fname =
  let basename  = Filename.basename (Filename.remove_extension c_fname) in
  let lit_fname = basename ^ ".litmus" in
  let asm_fname = basename ^ ".s" in
  let root      = !(cfg.out_root_path) in
  { c_path      = Filename.concat (c_path_of      results_path) c_fname
  ; litc_path   = Filename.concat (litc_path_of   results_path) lit_fname
  ; x86_path    = Filename.concat (x86_path_of    root)         asm_fname
  ; litx86_path = Filename.concat (litx86_path_of root)         lit_fname
  }

let run_cc (cc : string) (argv : string list) =
  let pid = Unix.create_process cc
                                (Array.of_list (cc :: argv))
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

let compile_x86 (ccspec : compiler_spec) (ps : pathset) =
  let final_argv = ccspec.argv @ ["-o"; ps.x86_path; "--"; ps.c_path] in
  run_cc ccspec.cc final_argv

let summarise_pathset (vf : Format.formatter) (ps : pathset) : unit =
  List.iter
    (fun (io, t, v) -> Format.fprintf vf "@[[%s] %s file:@ %s@]@." io t v)
    [ ("in" , "C"         , ps.c_path)
    ; ("in" , "C Litmus"  , ps.litc_path)
    ; ("out", "x86"       , ps.x86_path)
    ; ("out", "x86 Litmus", ps.litx86_path)
    ]

let proc_c_err (cfg : config) (ccspec : compiler_spec) vf results_path c_fname =
  let paths = gen_pathset cfg results_path c_fname in
  summarise_pathset vf paths;
  compile_x86 ccspec paths

let proc_c (cfg : config) (ccspec : compiler_spec) vf results_path c_fname : unit =
  match proc_c_err cfg ccspec vf results_path c_fname with
  | Ok _ -> ()
  | Error err ->
     Format.eprintf "@[error:@ %a@]@." R.pp_msg err

let proc_results (cfg : config) (ccspec : compiler_spec) (vf : Format.formatter) (results_path : string) : unit =
  let c_path = Filename.concat results_path "C" in
  try
    (Sys.readdir c_path)
    |> Array.to_seq
    |> Seq.filter (fun file -> Filename.extension file = ".c")
    |> Seq.iter (proc_c cfg ccspec vf results_path)
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
  Format.pp_close_box f ()

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
  Format.pp_print_cut f ();
  pp_kv "C compiler (x86) args" (pp_sq cfg.x86_argv) f;
  Format.pp_print_cut f ();
  pp_kv "memalloy results paths" (pp_sq cfg.results_paths) f;
  Format.pp_close_box f ();
  Format.pp_print_cut f ();
  Format.pp_close_box f ()

let null_formatter () : Format.formatter =
  Format.make_formatter (fun _ _ _ -> ()) (fun _ -> ())

let test_compiler (cc : string) =
  run_cc cc ["--version"]

(* TODO: this is very gcc-centric. *)
let default_compiler_argv : string list =
  [ "-S"       (* emit assembly *)
  ; "-fno-pic" (* don't emit position-independent code *)
  ]

let make_compiler_argv (argv : string Queue.t) : string list =
  default_compiler_argv @ List.of_seq (Queue.to_seq argv)

let make_compiler_spec (cc : string) (argv : string Queue.t) =
  test_compiler cc
  |> R.reword_error_msg (fun _ -> R.msg "Compiler test failed")
  |> R.map (fun _ -> { cc = cc
                     ; argv = make_compiler_argv argv
                     }
           )

type ent_type =
  | File
  | Dir
  | Nothing

let get_ent_type (path : string) : ent_type =
  try
    if Sys.is_directory path
    then Dir
    else File
  with
  | Sys_error _ ->
     (* TODO: is this _always_ an ENOENT? *)
     Nothing

let mkdir (path : string) =
  match get_ent_type path with
  | Dir -> R.ok ()
  | File -> R.error_msgf "%s exists, but is a file" path
  | Nothing ->
     try
       Unix.mkdir path 0o755;
       R.ok ()
     with
     | Unix.Unix_error (errno, func, arg) ->
        R.error_msgf "couldn't %s %s: %s"
                     func arg (Unix.error_message errno)

let collect_map (f : 'a -> 'b -> ('b, 'e) result)
                (init : 'b)
                (xs : 'a list)
    : ('b, 'e) result =
  List.fold_right (fun (v : 'a) (c : ('b, 'e) result) -> c >>= f v)
                  xs
                  (R.ok init)

let make_dir_structure (root : string) =
  try
    if Sys.is_directory root
    then
      collect_map (fun p _ -> mkdir p)
                  ()
                  [ x86_path_of root
                  ; litx86_path_of root
                  ]
    else
      R.error_msgf "%s not a directory" root
  with
  | Sys_error e -> R.error_msg e

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

  make_dir_structure !(cfg.out_root_path) |>
    R.reword_error_msg (fun _ -> R.msg "couldn't make dir structure")
  >>= (
    fun () ->
    make_compiler_spec !(cfg.x86_cc) cfg.x86_argv |>
      R.reword_error_msg (fun _ -> R.msg "invalid x86 compiler")
    >>= (
      fun x86_spec ->
      Queue.iter (proc_results cfg x86_spec verbose_fmt) cfg.results_paths;
      R.ok ()
    )
  )
  |>
    function
    | Ok _ -> ()
    | Error err ->
       Format.eprintf "@[Fatal error:@.@[%a@]@]@." R.pp_msg err

