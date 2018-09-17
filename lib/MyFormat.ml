open Core

let pp_kv (k : string) (v : Format.formatter -> unit) (f : Format.formatter) : unit =
  Format.pp_open_hvbox f 0;
  Format.pp_print_string f k;
  Format.pp_print_string f ":";
  Format.pp_print_break f 1 1;
  v f;
  Format.pp_close_box f ()

let pp_sr (s : string ref) (f : Format.formatter) : unit =
  Format.pp_print_string f !s

let pp_sq (sq : string Queue.t) (f : Format.formatter) : unit =
  let first = ref true in
  Queue.iter ~f:(fun s -> begin
                     if not !first then Format.pp_print_space f ();
                     first := false;
                     Format.pp_print_string f s
                   end)
             sq
