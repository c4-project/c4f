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

module type S = sig
  module Lang : Language.S

  type t =
    { name : string
    ; init : ((string, Lang.Constant.t) List.Assoc.t)
    ; programs : Lang.Statement.t list list
    }

  include Pretty_printer.S with type t := t

  val make
    :  name:string
    -> init:((string, Lang.Constant.t) List.Assoc.t)
    -> programs:Lang.Statement.t list list
    -> t Or_error.t
end

module Make (Lang : Language.S) : S with module Lang = Lang = struct
  module Lang = Lang

  type t =
    { name : string
    ; init : ((string, Lang.Constant.t) List.Assoc.t)
    ; programs : Lang.Statement.t list list
    }

  (** [validate_init init] validates an incoming litmus test's
     init block. *)
  let validate_init (init : (string, Lang.Constant.t) List.Assoc.t)
      : unit Or_error.t =
    let dup =
      List.find_a_dup ~compare:(fun x y -> String.compare (fst x) (fst y))
        init
    in
    let dup_to_err (k, v) =
      Or_error.error_s
        [%message "duplicate item in 'init'"
            ~location:k
            ~value:(v : Lang.Constant.t)
        ]
    in
    Option.value_map
      ~default:Result.ok_unit
      ~f:dup_to_err
      dup

  (** [validate_programs ps] validates an incoming litmus test's
     programs. *)
  let validate_programs = function
    | [] -> Or_error.error_string "programs are empty"
    | p::ps ->
       let l = List.length p in
       Result.ok_if_true
         (List.for_all ~f:(fun p' -> List.length p' = l) ps)
         ~error:(Error.of_string "programs must be of uniform size")
  ;;

  (** [validate lit] validates an incoming litmus test. *)
  let validate (lit : t) : unit Or_error.t =
    Or_error.combine_errors_unit
      [ validate_init lit.init
      ; validate_programs lit.programs
      ]

  let make ~name ~init ~programs =
    let open Or_error.Let_syntax in
    let lit = { name; init; programs } in
    let%map () = validate lit in
  lit
  ;;

  let pp_init (f : Format.formatter)
              (init : (string, Lang.Constant.t) List.Assoc.t)
    : unit =
    My_format.pp_c_braces
      f
      (fun f ->
         Format.pp_print_list
           ~pp_sep:Format.pp_print_space
           (fun f (l, c) ->
              Format.fprintf f "@[%s = %a;@]" l Lang.Constant.pp c
           )
           f
          init)

  let pp_instr (f : Format.formatter) =
      Format.fprintf f "@[<h>%a@]" Lang.Statement.pp

  let pp_programs (f : Format.formatter)
                  (ps : Lang.Statement.t list list)
      : unit =
    let program_names =
      List.mapi ~f:(fun i _ -> sprintf "P%d" i) ps
    in
    let header : Tabulator.row =
      List.map ~f:(Fn.flip String.pp) program_names
    in
    (* The [is_valid] check in [pp] guarantees this transpose is okay. *)
    let rows : Tabulator.row list =
      ps
      |> List.transpose_exn
      |> List.map ~f:(fun row -> List.map ~f:(Fn.flip pp_instr) row)
    in
    let result =
      let open Or_error in
      Tabulator.(
        make ~sep:" | " ~terminator:" ;" ~header ()
        >>= with_rows rows
        >>| pp f
      )
    in
    Result.iter_error result
      ~f:(fun e ->
          Format.fprintf f
            "@[<@ error printing table:@ %a@ >@]"
            Error.pp e)
  ;;

  let pp_location_stanza f init =
    Format.fprintf f "@[<h>locations@ [@[%a@]]@]@,"
      ( Format.pp_print_list
          ~pp_sep:(fun f () -> Format.fprintf f ";@ ")
          String.pp
      )
      (List.map ~f:fst init)

  let pp_body_validated f litmus =
    pp_init f (litmus.init);
    Format.pp_print_cut f ();
    Format.pp_print_cut f ();
    pp_programs f (litmus.programs);
    Format.pp_print_cut f ();
    Format.pp_print_cut f ();
    (* This just repeats information already in the initialiser,
       but herd7 seems to need either a location stanza or a
       precondition, and this is always guaranteed to exist. *)
    pp_location_stanza f (litmus.init)

  let pp_body f litmus =
    match validate litmus with
    | Ok _ -> pp_body_validated f litmus
    | Error err -> Format.fprintf f "[@(*@ @[<hov>Invalid litmus file:@ %a@] *)@]@,"
                     Error.pp err

  let pp f litmus =
    Format.pp_open_vbox f 0;
    Format.fprintf f "@[%s@ %s@]@,@," Lang.name litmus.name;
    pp_body f litmus;
    Format.pp_close_box f ()
end
