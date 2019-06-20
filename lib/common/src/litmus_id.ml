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
module Au = Act_utils
module Tx = Travesty_base_exts

(* Comparable.Make_plain depends on Sexpable, and Sexpable.Of_stringable
   depends on Stringable. As a result, we have to implement Id by
   snowballing together increasingly elaborate modules, adding Core_kernel
   extensions as we go. *)

module M_str = struct
  type t =
    | Local of Au.My_quickcheck.Small_non_negative_int.t * C_id.t
    | Global of C_id.t
  [@@deriving equal, variants, quickcheck]

  let compare (x : t) (y : t) =
    match (x, y) with
    | Global _, Local _ ->
        -1
    | Local _, Global _ ->
        1
    | Global x, Global y ->
        [%compare: C_id.t] x y
    | Local (xi, x), Local (yi, y) ->
        [%compare: int * C_id.t] (xi, x) (yi, y)

  let to_string : t -> string = function
    | Local (t, id) ->
        Printf.sprintf "%d:%s" t (C_id.to_string id)
    | Global id ->
        C_id.to_string id

  let try_parse_local (s : string) : (int * string) option =
    let open Option.Let_syntax in
    let%bind thread, rest = String.lsplit2 ~on:':' s in
    let%bind tnum = Caml.int_of_string_opt thread in
    let%map tnum = Option.some_if (Int.is_non_negative tnum) tnum in
    (tnum, rest)

  let try_parse (s : string) : t Or_error.t =
    match try_parse_local s with
    | Some (t, id) ->
        Or_error.(id |> C_id.create >>| local t)
    | None ->
        Or_error.(s |> C_id.create >>| global)

  let of_string (s : string) : t = Or_error.ok_exn (try_parse s)
end

module M_sexp = struct
  include M_str
  include Sexpable.Of_stringable (M_str)
end

include M_sexp
include Comparable.Make (M_sexp)

module On_c_identifiers :
  Travesty.Traversable.S0 with type t = t and type Elt.t = C_id.t =
Travesty.Traversable.Make0 (struct
  type nonrec t = t

  module Elt = C_id

  module On_monad (M : Monad.S) = struct
    module H = Travesty.Traversable.Helpers (M)

    let map_m (lid : t) ~(f : C_id.t -> C_id.t M.t) : t M.t =
      Variants.map lid ~global:(H.proc_variant1 f)
        ~local:
          (H.proc_variant2 (fun (tid, x) ->
               M.(x |> f >>| fun x' -> (tid, x')) ))
  end
end)

let global_of_string (str : string) : t Or_error.t =
  Or_error.(str |> C_id.create >>| global)

let variable_name : t -> C_id.t = function Local (_, v) | Global v -> v

let as_local : t -> (int * C_id.t) option = function
  | Local (i, v) ->
      Some (i, v)
  | Global _ ->
      None

let tid : t -> int option = Fn.compose (Option.map ~f:fst) as_local

let as_global : t -> C_id.t option = function
  | Global cid ->
      Some cid
  | Local _ ->
      None

let to_memalloy_id_inner (t : int) (id : C_id.t) : string =
  Printf.sprintf "t%d%s" t (C_id.to_string id)

let to_memalloy_id : t -> C_id.t = function
  | Local (t, id) ->
      C_id.of_string (to_memalloy_id_inner t id)
  | Global id ->
      id

let pp : t Fmt.t =
 fun f -> function
  | Local (tid, str) ->
      Fmt.pf f "%d:%a" tid C_id.pp str
  | Global str ->
      C_id.pp f str

module Assoc = struct
  type 'a t = (M_sexp.t, 'a) List.Assoc.t

  let split_initial (str : string) : string * string option =
    str |> String.rsplit2 ~on:'='
    |> Option.value_map
         ~f:(Tx.Tuple2.map_right ~f:Option.some)
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

let is_in_scope (id : t) ~(from : int) : bool =
  match id with Global _ -> true | Local (tid, _) -> Int.equal tid from
