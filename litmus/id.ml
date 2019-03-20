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

(* Don't use Base: it causes us to derive a weaker signature *)
open Core_kernel
open Utils

(* Comparable.Make_plain depends on Sexpable, and
   Sexpable.Of_stringable depends on Stringable.  As a result, we
   have to implement Id by snowballing together increasingly
   elaborate modules, adding Core_kernel extensions as we go. *)

module M_str = struct
  type t =
    | Local of My_quickcheck.Small_non_negative_int.t * C_identifier.t
    | Global of C_identifier.t
  [@@deriving compare, variants, quickcheck]

  let to_string : t -> string = function
    | Local (t, id) -> Printf.sprintf "%d:%s" t (C_identifier.to_string id)
    | Global id -> C_identifier.to_string id
  ;;

  let try_parse_local (s : string) : (int * string) option =
    let open Option.Let_syntax in
    let%bind thread, rest = String.lsplit2 ~on:':' s in
    let%bind tnum = Caml.int_of_string_opt thread in
    let%map tnum = Option.some_if (Int.is_non_negative tnum) tnum in
    tnum, rest
  ;;

  let try_parse (s : string) : t Or_error.t =
    match try_parse_local s with
    | Some (t, id) -> Or_error.(id |> C_identifier.create >>| local t)
    | None -> Or_error.(s |> C_identifier.create >>| global)
  ;;

  let of_string (s : string) : t = Or_error.ok_exn (try_parse s)
end

module M_sexp = struct
  include M_str
  include Sexpable.Of_stringable (M_str)
end

include M_sexp
include Comparable.Make (M_sexp)

let to_memalloy_id_inner (t : int) (id : C_identifier.t) : string =
  Printf.sprintf "t%d%s" t (C_identifier.to_string id)
;;

let%test_unit "to_memalloy_id_inner produces valid identifiers" =
  Base_quickcheck.Test.run_exn
    (module struct
       type t = My_quickcheck.Small_non_negative_int.t * C_identifier.t
       [@@deriving sexp, quickcheck]
    end)
    ~f:(fun (t, id) ->
      [%test_pred: C_identifier.t Or_error.t]
        ~here:[ [%here] ]
        Or_error.is_ok
        (C_identifier.create (to_memalloy_id_inner t id)))
;;

let to_memalloy_id : t -> C_identifier.t = function
  | Local (t, id) -> C_identifier.of_string (to_memalloy_id_inner t id)
  | Global id -> id
;;

let%test_module "Id tests" =
  (module struct
     let%test_unit "to_string->of_string is identity" =
       Base_quickcheck.Test.run_exn
         (module M_sexp)
         ~f:(fun ident ->
           [%test_eq: t] ~here:[ [%here] ] ident (of_string (to_string ident)))
     ;;

     let%test_unit "to_memalloy_id is identity on globals" =
       Base_quickcheck.Test.run_exn
         (module C_identifier)
         ~f:(fun ident ->
           [%test_eq: C_identifier.t]
             ~here:[ [%here] ]
             ident
             (to_memalloy_id (Global ident)))
     ;;
  end)
;;

let pp : t Fmt.t =
 fun f -> function
  | Local (tid, str) -> Fmt.pf f "%d:%a" tid C_identifier.pp str
  | Global str -> C_identifier.pp f str
;;
