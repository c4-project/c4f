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

include Ast_intf

module Make (Lang : Basic) : S with module Lang = Lang = struct
  module Lang = Lang

  type t =
    { name : string
    ; init : ((string, Lang.Constant.t) List.Assoc.t)
    ; programs : Lang.Statement.t list list
    } [@@deriving fields]
  ;;

  let make = Fields.create

  module Validated = struct
    type nonrec t = t
    let name = name
    let init = init
    let programs = programs

    let pp_init
      :  Formatter.t
        -> (string, Lang.Constant.t) List.Assoc.t
        -> unit =
      My_format.pp_c_braces
        (Fmt.(
            list ~sep:sp
              (fun f (l, c) -> pf f "@[%s = %a;@]" l Lang.Constant.pp c)
          )
        )
    ;;

    let pp_instr (f : Formatter.t) =
      Fmt.pf f "@[<h>%a@]" Lang.Statement.pp

    module Program_tabulator = struct
      module M  = struct
        type data = Lang.Statement.t list list

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
                      ~table:(programs : Lang.Statement.t list list)]
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

    let pp_programs_inner =
      Program_tabulator.pp_as_table
        ~on_error:(fun f e ->
            Fmt.pf f
              "@[<@ error printing table:@ %a@ >@]"
              Error.pp e)
    ;;

    let pp_programs f t = pp_programs_inner f t.programs

    let pp_location_stanza f init =
      Fmt.(
        pf f "@[<h>locations@ [@[%a@]]@]"
          (list ~sep:(fun f () -> pf f ";@ ") string)
      ) (List.map ~f:fst init)
    ;;

    let pp_body f litmus =
      pp_init f (litmus.init);
      Fmt.cut f ();
      Fmt.cut f ();
      pp_programs_inner f (litmus.programs);
      Fmt.cut f ();
      Fmt.cut f ();
      (* This just repeats information already in the initialiser,
         but herd7 seems to need either a location stanza or a
         precondition, and this is always guaranteed to exist. *)
      pp_location_stanza f (litmus.init)
    ;;

    let pp =
      Fmt.(
        vbox (
          fun f litmus ->
            Fmt.pf f "@[%s@ %s@]@,@," Lang.name litmus.name;
            pp_body f litmus
        )
      )
    ;;
  end

  (** [validate_init init] validates an incoming litmus test's
      init block. *)
  let validate_init (init : (string, Lang.Constant.t) List.Assoc.t) =
    let module Tr = Travesty in
    let module V = Validate in
    let dup =
      List.find_a_dup ~compare:(Tr.T_fn.on fst String.compare) init
    in
    let dup_to_err (k, v) =
      V.fail_s
        [%message "duplicate item in 'init'"
            ~location:k
            ~value:(v : Lang.Constant.t)
        ]
    in
    Option.value_map
      ~default:(V.pass)
      ~f:dup_to_err
      dup

  let is_uniform = function
    | [] -> true
    | p::ps ->
      let l = List.length p in
      List.for_all ~f:(fun p' -> List.length p' = l) ps
  ;;

  let validate_name =
    let module V = Validate in
    V.booltest (Fn.non String.is_empty) ~if_false:"name is empty"

  (** [validate_programs ps] validates an incoming litmus test's
      programs. *)
  let validate_programs =
    let module V = Validate in
    V.all
      [ V.booltest (Fn.non List.is_empty) ~if_false:"programs are empty"
      ; V.booltest is_uniform ~if_false:"programs must be uniform"
      ]
  ;;

  let validate_inner t =
    let module V = Validate in
    let w check = V.field_folder t check in
    V.of_list
      (Fields.fold ~init:[]
         ~name:(w validate_name)
         ~init:(w validate_init)
         ~programs:(w validate_programs)
      )
  ;;

  let validate lit : Validated.t Or_error.t =
    Validate.valid_or_error lit validate_inner
  ;;

  let make_and_validate ~name ~init ~programs =
    validate (make ~name ~init ~programs)
  ;;
end
