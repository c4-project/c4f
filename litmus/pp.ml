(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation
   files (the "Software"), to deal in the Software without
   restriction, including without limitation the rights to use, copy,
   modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

open Base
open Utils

include Pp_intf

module Make_tabular (Ast : Ast.S) : S with module Ast = Ast = struct
  module Ast = Ast

  let pp_instr (f : Formatter.t) =
    Fmt.pf f "@[<h>%a@]" Ast.Lang.Statement.pp

  module Program_tabulator = struct
    module M  = struct
      type data = Ast.Lang.Statement.t list list

      let to_table programs =
        let open Or_error.Let_syntax in

        let program_names =
          List.mapi ~f:(fun i _ -> Printf.sprintf "P%d" i) programs
        in
        let header : Tabulator.row =
          List.map ~f:(Fn.flip String.pp) program_names
        in

        let%bind programs' =
          Result.of_option (List.transpose programs)
            ~error:(
              Error.create_s
                [%message "Couldn't transpose program table"
                    ~table:(programs : Ast.Lang.Statement.t list list)]
            )
        in
        let rows =
          List.map ~f:(List.map ~f:(Fn.flip pp_instr)) programs'
        in

        Tabulator.(
          make ~sep:" | " ~terminator:" ;" ~header ()
          >>= with_rows rows
        )
    end

    include M
    include Tabulator.Extend_tabular (M)
  end

  let pp_listings : Ast.Lang.Statement.t list list Fmt.t =
    Program_tabulator.pp_as_table
      ~on_error:(fun f e ->
          Fmt.pf f
            "@[<@ error printing table:@ %a@ >@]"
            Error.pp e)
  ;;

  let pp_init : (string, Ast.Lang.Constant.t) List.Assoc.t Fmt.t =
    My_format.pp_c_braces
      (Fmt.(
          list ~sep:sp
            (fun f (l, c) -> pf f "@[%s = %a;@]" l Ast.Lang.Constant.pp c)
        )
      )
  ;;

  let pp_programs_inner : Ast.Lang.Program.t list Fmt.t =
    Fmt.using (List.map ~f:(Ast.Lang.Program.listing)) pp_listings

  let pp_programs : Ast.Validated.t Fmt.t =
    Fmt.using Ast.Validated.programs pp_programs_inner

  let pp_location_stanza f init =
    Fmt.(
      pf f "@[<h>locations@ [@[%a@]]@]"
        (list ~sep:(fun f () -> pf f ";@ ") string)
    ) (List.map ~f:fst init)
  ;;

  let pp_body f litmus =
    pp_init f (Ast.Validated.init litmus);
    Fmt.cut f ();
    Fmt.cut f ();
    pp_programs f litmus;
    Fmt.cut f ();
    Fmt.cut f ();
    (* This just repeats information already in the initialiser,
       but herd7 seems to need either a location stanza or a
       precondition, and this is always guaranteed to exist. *)
    pp_location_stanza f (Ast.Validated.init litmus)
  ;;

  let pp =
    Fmt.(
      vbox (
        fun f litmus ->
          Fmt.pf f "@[%s@ %s@]@,@,"
            Ast.Lang.name (Ast.Validated.name litmus);
          pp_body f litmus
      )
    )
  ;;
end
