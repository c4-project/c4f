(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to permit
   persons to whom the Software is furnished to do so, subject to the
   following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
   NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
   DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
   OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
   USE OR OTHER DEALINGS IN THE SOFTWARE. *)

open Base
open Stdio
module Ac = Act_common
open Pp_intf

module Generic = struct
  let pp_location_stanza : Ac.C_id.t list option Fmt.t =
    Fmt.(
      option
        (hbox
           (prefix (unit "locations@ ")
              (brackets (box (list ~sep:(unit ";@ ") Ac.C_id.pp))))))

  let rec pp_predicate (f : Formatter.t) (pred : 'const Ast_base.Pred.t)
      ~(pp_const : 'const Fmt.t) : unit =
    let mu = pp_predicate ~pp_const in
    match pred with
    | Bracket p ->
        Fmt.parens mu f p
    | Or (l, r) ->
        Fmt.pf f "%a@ \\/@ %a" mu l mu r
    | And (l, r) ->
        Fmt.pf f "%a@ /\\@ %a" mu l mu r
    | Elt (Eq (i, c)) ->
        Fmt.pf f "%a@ ==@ %a" Id.pp i pp_const c

  let pp_quantifier (f : Formatter.t) : [`Exists] -> unit = function
    | `Exists ->
        Fmt.string f "exists"

  let pp_post (f : Formatter.t) (pc : 'const Ast_base.Postcondition.t)
      ~(pp_const : 'const Fmt.t) : unit =
    let {Ast_base.Postcondition.quantifier; predicate} = pc in
    Fmt.(box (pair ~sep:sp pp_quantifier (parens (pp_predicate ~pp_const))))
      f (quantifier, predicate)
end

module type Basic = sig
  module Ast : Ast.S

  val print_programs_inner :
    Out_channel.t -> Ast.Lang.Program.t list -> unit
end

(** Makes the bits of a litmus AST that are common to all styles. *)
module Make_common (B : Basic) = struct
  let print_programs (oc : Out_channel.t) (ast : B.Ast.Validated.t) : unit =
    B.print_programs_inner oc (B.Ast.Validated.programs ast)

  let pp_init : (Ac.C_id.t, B.Ast.Lang.Constant.t) List.Assoc.t Fmt.t =
    Act_utils.My_format.pp_c_braces
      Fmt.(
        list ~sep:sp (fun f (l, c) ->
            pf f "@[%a = %a;@]" Ac.C_id.pp l B.Ast.Lang.Constant.pp c ))

  let pp_post : B.Ast.Lang.Constant.t Ast_base.Postcondition.t Fmt.t =
    Generic.pp_post ~pp_const:B.Ast.Lang.Constant.pp

  let print_body (oc : Out_channel.t) (litmus : B.Ast.Validated.t) : unit =
    let f = Caml.Format.formatter_of_out_channel oc in
    pp_init f (B.Ast.Validated.init litmus) ;
    Fmt.pf f "@.@." ;
    print_programs oc litmus ;
    Fmt.pf f "@." ;
    Generic.pp_location_stanza f (B.Ast.Validated.locations litmus) ;
    Fmt.(option (prefix (unit "@,@,") pp_post))
      f
      (B.Ast.Validated.postcondition litmus) ;
    Caml.Format.pp_print_flush f ()

  let print (oc : Out_channel.t) (litmus : B.Ast.Validated.t) : unit =
    let lang_name = B.Ast.Lang.name in
    let test_name = B.Ast.Validated.name litmus in
    Out_channel.fprintf oc "%s %s\n\n%a" lang_name test_name print_body
      litmus
end

module Make_tabular (Ast : Ast.S) : S with module Ast = Ast = struct
  module Ast = Ast

  module Specific = struct
    let pp_instr = Fmt.strf "@[<h>%a@]" Ast.Lang.Statement.pp

    module Program_tabulator = struct
      module M = struct
        type data = Ast.Lang.Statement.t list list

        let to_table programs =
          let open Or_error.Let_syntax in
          let header =
            List.mapi ~f:(fun i _ -> Printf.sprintf "P%d" i) programs
          in
          let%bind programs' =
            Result.of_option (List.transpose programs)
              ~error:
                (Error.create_s
                   [%message
                     "Couldn't transpose program table"
                       ~table:(programs : Ast.Lang.Statement.t list list)])
          in
          let rows = List.map ~f:(List.map ~f:pp_instr) programs' in
          Act_utils.Tabulator.(
            make ~sep:" | " ~terminator:" ;" ~header () >>= add_rows ~rows)
      end

      include M
      include Act_utils.Tabulator.Extend_tabular (M)
    end

    let print_listings (oc : Out_channel.t) :
        Ast.Lang.Statement.t list list -> unit =
      Program_tabulator.print_as_table ~oc ~on_error:(fun e ->
          Fmt.epr "@[<@ error printing table:@ %a@ >@]" Error.pp e )

    let get_uniform_listings (progs : Ast.Lang.Program.t list) :
        Ast.Lang.Statement.t list list =
      progs
      |> List.map ~f:Ast.Lang.Program.listing
      |> Ast.Lang.Statement.make_uniform

    let print_programs_inner (oc : Out_channel.t)
        (programs : Ast.Lang.Program.t list) : unit =
      programs |> get_uniform_listings |> print_listings oc
  end

  include Make_common (struct
    module Ast = Ast
    include Specific
  end)
end

module Make_sequential (Ast : Ast.S) : S with module Ast = Ast = struct
  module Ast = Ast

  module Specific = struct
    let print_programs_inner (oc : Stdio.Out_channel.t) :
        Ast.Lang.Program.t list -> unit =
      Fmt.list ~sep:(Fmt.unit "@ @ ") Ast.Lang.Program.pp
        (Caml.Format.formatter_of_out_channel oc)
  end

  include Make_common (struct
    module Ast = Ast
    include Specific
  end)
end
