open Core

let null_formatter () =
  Format.make_formatter (fun _ _ _ -> ()) (fun _ -> ())

let pp_option f ~pp = Option.iter ~f:(pp f)

let pp_csep f () = Format.fprintf f ",@ "

let pp_listlist ~pp f =
  Format.fprintf f "@[<v>%a@]"
    (Format.pp_print_list
       ~pp_sep:Format.pp_print_cut
       (fun f k ->
          Format.fprintf f "@[<h>[@ %a@ ]@]"
            (Format.pp_print_list
               ~pp_sep:pp_csep pp)
            k))

let pp_c_braces f pi =
  Format.pp_open_vbox f 4;
  Format.pp_print_char f '{';
  Format.pp_print_cut f ();
  pi f;
  Format.pp_close_box f ();
  Format.pp_print_cut f ();
  Format.pp_print_char f '}'

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
  Queue.iteri ~f:(fun i s -> begin
                     if 0 < i then Format.pp_print_space f ();
                     Format.pp_print_string f s
                   end)
             sq

let format_to_string pp v =
  let buf = Buffer.create 16 in
  let f = Format.formatter_of_buffer buf in
  Format.pp_open_box f 0;
  pp f v;
  Format.pp_close_box f ();
  Format.pp_print_flush f ();
  Buffer.contents buf
