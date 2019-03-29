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

(* Don't use Base: it causes us to derive a weaker signature *)
open Core_kernel
open Utils

(* Comparable.Make_plain depends on Sexpable, and Sexpable.Of_stringable
   depends on Stringable. As a result, we have to implement Id by
   snowballing together increasingly elaborate modules, adding Core_kernel
   extensions as we go. *)

module M_str = struct
  type t =
    | Local of My_quickcheck.Small_non_negative_int.t * C_identifier.t
    | Global of C_identifier.t
  [@@deriving compare, variants, quickcheck]

  let to_string : t -> string = function
    | Local (t, id) ->
        Printf.sprintf "%d:%s" t (C_identifier.to_string id)
    | Global id ->
        C_identifier.to_string id

  let try_parse_local (s : string) : (int * string) option =
    let open Option.Let_syntax in
    let%bind thread, rest = String.lsplit2 ~on:':' s in
    let%bind tnum = Caml.int_of_string_opt thread in
    let%map tnum = Option.some_if (Int.is_non_negative tnum) tnum in
    (tnum, rest)

  let try_parse (s : string) : t Or_error.t =
    match try_parse_local s with
    | Some (t, id) ->
        Or_error.(id |> C_identifier.create >>| local t)
    | None ->
        Or_error.(s |> C_identifier.create >>| global)

  let of_string (s : string) : t = Or_error.ok_exn (try_parse s)
end

module M_sexp = struct
  include M_str
  include Sexpable.Of_stringable (M_str)
end

include M_sexp
include Comparable.Make (M_sexp)

let%expect_test "try_parse: example local identifier" =
  Stdio.print_s [%sexp (try_parse "0:r1" : t Or_error.t)] ;
  [%expect {| (Ok 0:r1) |}]

let%expect_test "try_parse: example global identifier" =
  Stdio.print_s [%sexp (try_parse "x" : t Or_error.t)] ;
  [%expect {| (Ok x) |}]

let%expect_test "try_parse: example invalid identifier" =
  Stdio.print_s [%sexp (try_parse "0:1" : t Or_error.t)] ;
  [%expect
    {|
    (Error
     ("validation failed"
      (1 ("validation errors" (("fst.char '1'" "Invalid initial character.")))
       utils/c_identifier.ml:57:13))) |}]

let global_of_string (str : string) : t Or_error.t =
  Or_error.(str |> C_identifier.create >>| global)

let variable_name : t -> C_identifier.t = function
  | Local (_, v) | Global v ->
      v

let tid : t -> int option = function
  | Local (i, _) ->
      Some i
  | Global _ ->
      None

let as_global : t -> C_identifier.t option = function
  | Global cid ->
      Some cid
  | Local _ ->
      None

let to_memalloy_id_inner (t : int) (id : C_identifier.t) : string =
  Printf.sprintf "t%d%s" t (C_identifier.to_string id)

let%test_unit "to_memalloy_id_inner produces valid identifiers" =
  Base_quickcheck.Test.run_exn
    ( module struct
      type t = My_quickcheck.Small_non_negative_int.t * C_identifier.t
      [@@deriving sexp, quickcheck]
    end )
    ~f:(fun (t, id) ->
      [%test_pred: C_identifier.t Or_error.t] ~here:[[%here]] Or_error.is_ok
        (C_identifier.create (to_memalloy_id_inner t id)) )

let to_memalloy_id : t -> C_identifier.t = function
  | Local (t, id) ->
      C_identifier.of_string (to_memalloy_id_inner t id)
  | Global id ->
      id

let%test_module "Id tests" =
  ( module struct
    let%test_unit "to_string->of_string is identity" =
      Base_quickcheck.Test.run_exn
        (module M_sexp)
        ~f:(fun ident ->
          [%test_eq: t] ~here:[[%here]] ident (of_string (to_string ident))
          )

    let%test_unit "to_memalloy_id is identity on globals" =
      Base_quickcheck.Test.run_exn
        (module C_identifier)
        ~f:(fun ident ->
          [%test_eq: C_identifier.t] ~here:[[%here]] ident
            (to_memalloy_id (Global ident)) )
  end )

let pp : t Fmt.t =
 fun f -> function
  | Local (tid, str) ->
      Fmt.pf f "%d:%a" tid C_identifier.pp str
  | Global str ->
      C_identifier.pp f str

module Assoc = struct
  type 'a t = (M_sexp.t, 'a) List.Assoc.t

  let split_initial (str : string) : string * string option =
    str |> String.rsplit2 ~on:'='
    |> Option.value_map
         ~f:(Tuple2.map_snd ~f:Option.some)
         ~default:(str, None)

  let split_and_strip_initial (str : string) : string * string option =
    let name_str_unstripped, value_str_unstripped = split_initial str in
    let name_str = String.strip name_str_unstripped in
    let value_str = Option.map ~f:String.strip value_str_unstripped in
    (name_str, value_str)

  let%expect_test "split_and_strip_initial: present" =
    Stdio.print_s
      [%sexp
        (split_and_strip_initial "foo = barbaz" : string * string option)] ;
    [%expect {| (foo (barbaz)) |}]

  let%expect_test "split_and_strip_initial: absent" =
    Stdio.print_s
      [%sexp (split_and_strip_initial "foobar" : string * string option)] ;
    [%expect {| (foobar ()) |}]

  let%expect_test "split_and_strip_initial: double equals" =
    Stdio.print_s
      [%sexp
        (split_and_strip_initial "foo=bar=baz" : string * string option)] ;
    [%expect {| (foo=bar (baz)) |}]

  let try_parse_pair ~(value_parser : string option -> 'a Or_error.t)
      (str : string) : (M_sexp.t * 'a) Or_error.t =
    let open Or_error.Let_syntax in
    let name_str, value_str_opt = split_and_strip_initial str in
    let%bind name = try_parse name_str in
    let%map value = value_parser value_str_opt in
    (name, value)

  let try_parse (strs : string list)
      ~(value_parser : string option -> 'a Or_error.t) : 'a t Or_error.t =
    strs
    |> List.map ~f:(try_parse_pair ~value_parser)
    |> Or_error.combine_errors
end
