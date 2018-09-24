(* This file is part of 'act'.

Copyright (c) 2018 by Matt Windsor

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. *)

open Core

type 'a t =
  { lang : Language.t
  ; name : string
  ; programs : 'a list list
  }

type err =
  | NameEmpty
  | ProgramsEmpty
  | ProgramsNotUniform

let str_err = function
  | NameEmpty -> "empty name given"
  | ProgramsEmpty -> "no programs given"
  | ProgramsNotUniform -> "programs have different sizes"

let pp_err f str = Format.pp_print_string f (str_err str)

let validate_programs (ps : 'a list) : err option =
  match ps with
  | [] -> Some ProgramsEmpty
  | p::ps ->
     let l = List.length p in
     if (List.for_all ~f:(fun p' -> List.length p' = l) ps)
     then None
     else Some ProgramsNotUniform

let make ~name ~lang ~programs =
  let lit = { name; lang; programs } in
  Option.value_map
    ~default:(Ok lit)
    ~f:Result.fail
    (validate_programs programs)

let pp_programs (ppa : Format.formatter -> 'a -> unit)
                (f : Format.formatter)
                (ps : 'a list list)
    : unit =
  let endl () =
    Format.pp_print_char f ';';
    Format.pp_print_tab f ()
  in

  let print_sep i =
    if 0 < i then Format.fprintf f "|@."
  in

  let print_header i _ =
    print_sep i;
    Format.fprintf f "@[P%d@]" i;
    Format.pp_set_tab f ()
  in

  let print_instr i (ins : 'a) =
    print_sep i;
    Format.fprintf f "@[%a@]" ppa ins;
    Format.pp_print_tab f ()
  in

  let print_row (row : 'a list) =
    List.iteri ~f:print_instr row;
    endl ();
  in

  Format.pp_open_tbox f ();

  List.iteri ~f:print_header ps;
  endl ();

  (* The [is_valid] check in [pp] guarantees this transpose is okay. *)
  let ps' = List.transpose_exn ps in
  List.iter ~f:print_row ps';

  Format.pp_close_tbox f ()

let pp ppa f litmus =
  Format.pp_open_vbox f 0;
  Format.fprintf f "@[%a@ %s@]@."
                 (Language.pp ~show_sublang:false) litmus.lang
                 litmus.name;

  match validate_programs (litmus.programs) with
  | None -> pp_programs ppa f (litmus.programs)
  | Some err -> Format.fprintf f "@[(* Invalid litmus file: %a *)@]@."
                               pp_err err;

  Format.pp_close_box f ()
