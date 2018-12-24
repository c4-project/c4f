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

module type Basic = sig
  val name : string

  module Constant : sig
    type t [@@deriving sexp]
    include Pretty_printer.S with type t := t
  end

  module Statement : sig
    type t [@@deriving sexp]
    include Pretty_printer.S with type t := t
  end
end

module type S = sig
  module Lang : Basic

  type t =
    { name : string
    ; init : ((string, Lang.Constant.t) List.Assoc.t)
    ; programs : Lang.Statement.t list list
    }

  include Pretty_printer.S with type t := t
  val pp_programs : Formatter.t -> t -> unit

  val make
    :  name:string
    -> init:((string, Lang.Constant.t) List.Assoc.t)
    -> programs:Lang.Statement.t list list
    -> t Or_error.t
end

module Make (Lang : Basic) : S with module Lang = Lang = struct
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

  let pp_body_validated f litmus =
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

  let pp_body f litmus =
    match validate litmus with
    | Ok _ -> pp_body_validated f litmus
    | Error err -> Fmt.pf f "[@(*@ @[<hov>Invalid litmus file:@ %a@] *)@]@,"
                     Error.pp err

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
