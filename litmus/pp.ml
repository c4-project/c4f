(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

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

module type Basic = sig
  module Ast : Ast.S

  val pp_programs_inner : Ast.Lang.Program.t list Fmt.t
end

(** Makes the bits of a litmus AST that are common to all styles. *)
module Make_common (B : Basic) = struct
  let pp_programs : B.Ast.Validated.t Fmt.t =
    Fmt.using B.Ast.Validated.programs B.pp_programs_inner

  let pp_init : (C_identifier.t, B.Ast.Lang.Constant.t) List.Assoc.t Fmt.t =
    My_format.pp_c_braces
      (Fmt.(
          list ~sep:sp
            (fun f (l, c) -> pf f "@[%a = %a;@]"
                C_identifier.pp l
                B.Ast.Lang.Constant.pp c)
        )
      )
  ;;

  let pp_location_stanza : C_identifier.t list option Fmt.t =
    Fmt.(
      option
        (hbox
           (prefix
             (unit "locations@ ")
             (brackets (box (list ~sep:(unit ";@ ") C_identifier.pp)))
           )
        )
    )
  ;;

  let pp_quantifier f = function
    | `Exists -> Fmt.string f "exists"
  ;;

  let rec pp_predicate f : B.Ast.Pred.t -> unit = function
    | Bracket pred -> Fmt.parens pp_predicate f pred
    | Or  (l, r) -> Fmt.pf f "%a@ \\/@ %a" pp_predicate l pp_predicate r
    | And (l, r) -> Fmt.pf f "%a@ /\\@ %a" pp_predicate l pp_predicate r
    | Elt (Eq (i, c)) ->
      Fmt.pf f "%a@ ==@ %a" Id.pp i B.Ast.Lang.Constant.pp c
  ;;

  let pp_post f { Ast_base.Postcondition.quantifier; predicate } =
    Fmt.(box (pair ~sep:sp pp_quantifier (parens pp_predicate)))
      f (quantifier, predicate)
  ;;

  let pp_body f (litmus : B.Ast.Validated.t) =
    Fmt.(
      pf f "%a@,@,%a@,@,%a%a"
        pp_init (B.Ast.Validated.init litmus)
        pp_programs litmus
        pp_location_stanza (B.Ast.Validated.locations litmus)
        (option (prefix (unit "@,@,") pp_post))
        (B.Ast.Validated.postcondition litmus)
    )
  ;;

  let pp =
    Fmt.(
      vbox (
        fun f litmus ->
          Fmt.pf f "@[%s@ %a@]@,@,"
            B.Ast.Lang.name
            string (B.Ast.Validated.name litmus);
          pp_body f litmus
      )
    )
  ;;
end

module Make_tabular (Ast : Ast.S) : S with module Ast = Ast = struct
  module Ast = Ast

  module Specific = struct
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

    let get_uniform_listings (progs : Ast.Lang.Program.t list)
        : Ast.Lang.Statement.t list list =
      progs
      |> List.map ~f:Ast.Lang.Program.listing
      |> Ast.Lang.Statement.make_uniform
    ;;

    let pp_programs_inner : Ast.Lang.Program.t list Fmt.t =
      Fmt.using get_uniform_listings pp_listings
  end
  include Make_common (struct
      module Ast = Ast
      include Specific
    end)
  ;;
end

module Make_sequential (Ast : Ast.S) : S with module Ast = Ast = struct
  module Ast = Ast

  module Specific = struct
    let pp_programs_inner : Ast.Lang.Program.t list Fmt.t =
      Fmt.list ~sep:(Fmt.unit "@ @ ") Ast.Lang.Program.pp
  end

  include Make_common (struct
      module Ast = Ast
      include Specific
    end)
  ;;
end
