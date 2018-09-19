open Core

let null_formatter () =
  Format.make_formatter (fun _ _ _ -> ()) (fun _ -> ())

let pp_kv f k pv v =
  Format.pp_open_hvbox f 0;
  Format.pp_print_string f k;
  Format.pp_print_string f ":";
  Format.pp_print_break f 1 1;
  pv f v;
  Format.pp_close_box f ()

let pp_sr f sr =
  Format.pp_print_string f !sr

let pp_sq f sq =
  let first = ref true in
  Queue.iter ~f:(fun s -> begin
                     if not !first then Format.pp_print_space f ();
                     first := false;
                     Format.pp_print_string f s
                   end)
             sq
