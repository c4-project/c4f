open Rresult

type config =
  {
    results_paths : string Queue.t;
  }

let usage = "act [paths to comparator output]"

let c_path_of results_path = Filename.concat results_path "C"
let lit_path_of results_path = Filename.concat results_path "litmus"

let proc_c_err results_path c_fname =
  let basename = Filename.basename (Filename.remove_extension c_fname) in
  let c_fpath = Filename.concat (c_path_of results_path) c_fname in
  let lit_fname = basename ^ ".litmus" in
  let lit_fpath = Filename.concat (lit_path_of results_path) lit_fname in
  Format.eprintf "@[C file:@ %s@]@.Litmus file:@ %s@]@." c_fpath lit_fpath;
  R.ok ()

let proc_c results_path c_fname : unit =
  match proc_c_err results_path c_fname with
  | Ok _ -> ()
  | Error err ->
     Format.eprintf "@[error:@ %a@]@." R.pp_msg err

let proc_results results_path : unit =
  let c_path = Filename.concat results_path "C" in
  try
    let c_files = Sys.readdir c_path in
    Array.iter (proc_c results_path) c_files
  with
    Sys_error e ->
     Format.eprintf "@[system error:@ %s@]@." e

let () =
  let argv_config : config =
    {
      results_paths = Queue.create ()
    }
  in

  let spec =
    [
    ]
  in

  let anon_fun (arg : string) =
    Queue.push arg argv_config.results_paths
  in

  Arg.parse spec anon_fun usage;

  Queue.iter proc_results argv_config.results_paths
