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
open Utils
open Utils.MyContainers

(*
 * Litmus AST module
 *)

module T (LS : Language.S) = struct

  type t =
    { name : string
    ; init : ((LS.location, LS.constant) List.Assoc.t)
    ; programs : LS.Statement.t list list
    }

  type err =
    | NameEmpty
    | ProgramsEmpty
    | ProgramsNotUniform
    | DuplicateInit of LS.location

  let pp_err f =
    function
    | NameEmpty ->
       Format.pp_print_string f "empty name given"
    | ProgramsEmpty ->
       Format.pp_print_string f "no programs given"
    | ProgramsNotUniform ->
       Format.pp_print_string f "programs have different sizes"
    | DuplicateInit (loc) ->
       Format.fprintf f "location %a initialised multiple times"
                      LS.pp_location loc

  (** [validate_init init] validates an incoming litmus test's
     init block. *)

  let validate_init (init : (LS.location, LS.constant) List.Assoc.t)
      : err option =
    List.find_a_dup ~compare:(fun x y -> if fst x = fst y then 0 else -1) init
    |> Option.map ~f:(fun x -> DuplicateInit (fst x))

  (** [validate_programs ps] validates an incoming litmus test's
     programs. *)

  let validate_programs (ps : LS.Statement.t list list) : err option =
    match ps with
    | [] -> Some ProgramsEmpty
    | p::ps ->
       let l = List.length p in
       if (List.for_all ~f:(fun p' -> List.length p' = l) ps)
       then None
       else Some ProgramsNotUniform

  (** [validate lit] validates an incoming litmus test. *)

  let validate (lit : t) : err option =
    Option.first_some
      (validate_init lit.init)
      (validate_programs lit.programs)

  let make ~name ~init ~programs =
    let lit = { name; init; programs } in
    Option.value_map ~default:(Ok lit) ~f:Result.fail (validate lit)

  let pp_init (f : Format.formatter)
              (init : (LS.location, LS.constant) List.Assoc.t)
    : unit =
    MyFormat.pp_c_braces
      f
      (fun f ->
        List.iter
          ~f:(fun (l, c) -> Format.fprintf f
                                           "@[%a = %a;@]@,"
                                           LS.pp_location l
                                           LS.pp_constant c)
          init)

  let pp_instr_raw (f : Format.formatter) =
      Format.fprintf f "@[<h>%a@]" LS.Statement.pp

  let instr_width (ins : LS.Statement.t) : int =
    String.length (MyFormat.format_to_string pp_instr_raw ins)

  let column_width : LS.Statement.t list list -> int =
    MyList.max_measure
      ~measure:(MyList.max_measure ~measure:instr_width)

  let pp_programs (f : Format.formatter)
                  (ps : LS.Statement.t list list)
      : unit =
    let cw = column_width ps in

    let endl () =
      Format.pp_print_char f ';';
      Format.pp_print_tab f ()
    in

    let pp_sep i =
      (* No need to left-pad the |: the way we do the column
         justification below does it for us *)
      if 0 < i then Format.fprintf f "| "
    in

    let pp_header i _ =
      pp_sep i;
      (* Since we're setting tabs here, we need to pad the column width.
         NB: the width of each column will be around 'cw+1', factoring
         in the 'P' prefix. *)
      Format.fprintf f "@[P%-*d@]" cw i;
      Format.pp_set_tab f ()
    in

    let pp_instr i (ins : 'a) =
      pp_sep i;
      pp_instr_raw f ins;
      Format.pp_print_tab f ()
    in

    let pp_row (row : 'a list) =
      List.iteri ~f:pp_instr row;
      endl ();
    in

    Format.pp_open_tbox f ();

    Format.pp_set_tab f ();
    List.iteri ~f:pp_header ps;
    endl ();

    (* The [is_valid] check in [pp] guarantees this transpose is okay. *)
    let ps' = List.transpose_exn ps in
    List.iter ~f:pp_row ps';

    Format.pp_close_tbox f ()

  let pp_body_validated f litmus =
    pp_init f (litmus.init);
    Format.pp_print_cut f ();
    Format.pp_print_cut f ();
    pp_programs f (litmus.programs)

  let pp_body f litmus =
    match validate litmus with
    | None -> pp_body_validated f litmus
    | Some err -> Format.fprintf f "@[(* Invalid litmus file: %a *)@]@," pp_err err

  let pp f litmus =
    Format.pp_open_vbox f 0;
    Format.fprintf f "@[%a@ %s@]@,@,"
                   (Language.pp_name ~show_sublang:false) LS.name
                   litmus.name;
    pp_body f litmus;
    Format.pp_close_box f ()
end
