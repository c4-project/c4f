open Core

let pp_kv (f : Format.formatter) (k : string) (vp : Format.formatter -> 'v -> unit) (v : 'v) : unit =
  Format.pp_open_hvbox f 0;
  Format.pp_print_string f k;
  Format.pp_print_string f ":";
  Format.pp_print_break f 1 1;
  vp f v;
  Format.pp_close_box f ()

let pp_sr (f : Format.formatter) (s : string ref) : unit =
  Format.pp_print_string f !s

let pp_sq (f : Format.formatter) (sq : string Queue.t) : unit =
  let first = ref true in
  Queue.iter ~f:(fun s -> begin
                     if not !first then Format.pp_print_space f ();
                     first := false;
                     Format.pp_print_string f s
                   end)
             sq
