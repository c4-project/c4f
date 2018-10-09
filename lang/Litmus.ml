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

module type Intf =
sig
  module LS : Language.Intf

  type t =
    { name : string
    ; init : ((LS.Location.t, LS.Constant.t) List.Assoc.t)
    ; programs : LS.Statement.t list list
    }

  include Pretty_printer.S with type t := t

  val make : name:string
             -> init:((LS.Location.t, LS.Constant.t) List.Assoc.t)
             -> programs:LS.Statement.t list list
             -> t Or_error.t
end

module Make (LS : Language.Intf) =
struct
  module LS = LS

  type t =
    { name : string
    ; init : ((LS.Location.t, LS.Constant.t) List.Assoc.t)
    ; programs : LS.Statement.t list list
    }

  (** [validate_init init] validates an incoming litmus test's
     init block. *)

  let validate_init (init : (LS.Location.t, LS.Constant.t) List.Assoc.t)
      : unit Or_error.t =
    let dup =
      List.find_a_dup ~compare:(fun x y -> if fst x = fst y then 0 else -1)
        init
    in
    let dup_to_err d =
      Or_error.error "duplicate item in 'init'" d [%sexp_of: LS.Location.t * LS.Constant.t]
    in
    Option.value_map
      ~default:Result.ok_unit
      ~f:dup_to_err
      dup

  (** [validate_programs ps] validates an incoming litmus test's
     programs. *)

  let validate_programs (ps : LS.Statement.t list list) : unit Or_error.t =
    match ps with
    | [] -> Or_error.error_string "programs are empty"
    | p::ps ->
       let l = List.length p in
       Result.ok_if_true
         (List.for_all ~f:(fun p' -> List.length p' = l) ps)
         ~error:(Error.of_string "programs must be of uniform size")

  (** [validate lit] validates an incoming litmus test. *)

  let validate (lit : t) : unit Or_error.t =
    Or_error.combine_errors_unit
      [ validate_init lit.init
      ; validate_programs lit.programs
      ]

  let make ~name ~init ~programs =
    let open Or_error in
    let lit = { name; init; programs } in
    validate lit *> return lit

  let pp_init (f : Format.formatter)
              (init : (LS.Location.t, LS.Constant.t) List.Assoc.t)
    : unit =
    MyFormat.pp_c_braces
      f
      (fun f ->
        List.iter
          ~f:(fun (l, c) -> Format.fprintf f
                                           "@[%a = %a;@]@,"
                                           LS.Location.pp l
                                           LS.Constant.pp c)
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
    | Ok _ -> pp_body_validated f litmus
    | Error err -> Format.fprintf f "[@(*@ @[<hov>Invalid litmus file:@ %a@] *)@]@,"
                     Error.pp err

  let pp f litmus =
    Format.pp_open_vbox f 0;
    Format.fprintf f "@[%a@ %s@]@,@,"
                   (Language.pp_name ~show_sublang:false) LS.name
                   litmus.name;
    pp_body f litmus;
    Format.pp_close_box f ()
end
