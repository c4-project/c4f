(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the "Software"),
   to deal in the Software without restriction, including without limitation
   the rights to use, copy, modify, merge, publish, distribute, sublicense,
   and/or sell copies of the Software, and to permit persons to whom the
   Software is furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
   THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
   DEALINGS IN THE SOFTWARE. *)

open Base
open Redirect_map_intf
module Alist = Travesty_base_exts.Alist

module Make (B : Basic_symbol) : S with module Sym = B = struct
  module Sym = B

  type t = B.t Map.M(B).t [@@deriving sexp]

  let dest_of_sym (map : t) (sym : B.t) : B.t =
    Option.value (Map.find map sym) ~default:sym

  let dest_of_id (map : t) (id : C_id.t) : C_id.t Or_error.t =
    let open Or_error.Let_syntax in
    let%bind sym = B.of_c_identifier id in
    let redirected_sym = dest_of_sym map sym in
    B.to_c_identifier redirected_sym

  let dest_syms (map : t) ~(sources : Set.M(B).t) : Set.M(B).t =
    (* We can't just take the image of 'sources' in 'map', as this would miss
       out identity mappings. *)
    sources |> Set.to_list
    |> List.map ~f:(dest_of_sym map)
    |> Set.of_list (module B)

  let dest_ids (map : t) ~(sources : Set.M(C_id).t) :
      Set.M(C_id).t Or_error.t =
    let open Or_error.Let_syntax in
    let%bind source_sym_list =
      sources |> Set.to_list
      |> List.map ~f:B.of_c_identifier
      |> Or_error.combine_errors
    in
    let source_syms = Set.of_list (module B) source_sym_list in
    let dest_syms = dest_syms map ~sources:source_syms in
    let%map dest_id_list =
      dest_syms |> Set.to_list
      |> List.map ~f:B.to_c_identifier
      |> Or_error.combine_errors
    in
    Set.of_list (module C_id) dest_id_list

  let of_symbol_alist : (B.t, B.t) List.Assoc.t -> t Or_error.t =
    Map.of_alist_or_error (module B)

  let to_string_alist (map : t) : (string, string) List.Assoc.t =
    map |> Map.to_alist |> Alist.bi_map ~left:B.to_string ~right:B.to_string

  let sources_of_sym (map : t) (dest : B.t) : Set.M(B).t =
    map
    |> Map.filter ~f:([%equal: B.t] dest)
    |> Map.keys
    |> Set.of_list (module B)

  let propagate (src : B.t) (new_dst : B.t) (old_dst : B.t) : B.t =
    if [%equal: B.t] old_dst src then new_dst else old_dst

  let redirect ~src ~dst map =
    let map' = Map.set map ~key:src ~data:dst in
    if [%equal: B.t] src dst then map'
    else Map.map map' ~f:(propagate src dst)

  let identity () : t = Map.empty (module B)
end

let%test_module "string redirect maps" =
  ( module struct
    module M = Make (struct
      include Core_kernel.String

      let to_string = Fn.id

      let of_string = Fn.id

      let of_c_identifier x = x |> C_id.to_string |> Or_error.return

      let to_c_identifier = C_id.create
    end)

    let test_map : M.t =
      M.(
        identity ()
        |> redirect ~src:"alpha" ~dst:"alpha"
        |> redirect ~src:"bravo" ~dst:"_echo"
        |> redirect ~src:"charlie" ~dst:"_echo"
        |> redirect ~src:"_echo" ~dst:".foxtrot"
        |> redirect ~src:"hotel" ~dst:"_hotel"
        |> redirect ~src:"%delta" ~dst:"$kilo"
        |> redirect ~src:"$kilo" ~dst:"%delta")

    let%expect_test "dest_syms example run" =
      let sources =
        Core_kernel.String.Set.of_list
          ["alpha"; "bravo"; "charlie"; "%delta"; "_echo"; "hotel"; "whiskey"]
      in
      let r = M.dest_syms test_map ~sources in
      Stdio.print_s [%sexp (r : Core_kernel.String.Set.t)] ;
      [%expect {| (%delta .foxtrot _hotel alpha whiskey) |}]

    let%expect_test "dest_of_sym example run" =
      M.(
        let r =
          let alpha = dest_of_sym test_map "alpha" in
          let bravo = dest_of_sym test_map "bravo" in
          let charlie = dest_of_sym test_map "charlie" in
          let delta = dest_of_sym test_map "%delta" in
          (alpha, bravo, charlie, delta)
        in
        Stdio.print_s [%sexp (r : string * string * string * string)]) ;
      [%expect {| (alpha .foxtrot .foxtrot %delta) |}]

    let%expect_test "String.R_map: sources_of example run" =
      M.(
        let r =
          let alpha = sources_of_sym test_map "alpha" in
          let bravo = sources_of_sym test_map "bravo" in
          let echo = sources_of_sym test_map "_echo" in
          let foxtrot = sources_of_sym test_map ".foxtrot" in
          (alpha, bravo, echo, foxtrot)
        in
        Stdio.print_s
          [%sexp
            ( r
              : Core_kernel.String.Set.t
                * Core_kernel.String.Set.t
                * Core_kernel.String.Set.t
                * Core_kernel.String.Set.t )]) ;
      [%expect {| ((alpha) () () (_echo bravo charlie)) |}]

    let%expect_test "dest_of_id: in map, valid ID" =
      let foo = C_id.of_string "hotel" in
      Stdio.print_s [%sexp (M.dest_of_id test_map foo : C_id.t Or_error.t)] ;
      [%expect {| (Ok _hotel) |}]

    let%expect_test "dest_of_id: in map, invalid ID" =
      let foo = C_id.of_string "_echo" in
      Stdio.print_s [%sexp (M.dest_of_id test_map foo : C_id.t Or_error.t)] ;
      [%expect
        {|
        (Error
         ("validation failed"
          (.foxtrot
           ("validation errors" (("initial char '.'" "Invalid initial character.")))
           lib/common/src/c_id.ml:118:13))) |}]

    let%expect_test "dest_of_id: not in map" =
      let foo = C_id.of_string "nope" in
      Stdio.print_s [%sexp (M.dest_of_id test_map foo : C_id.t Or_error.t)] ;
      [%expect {| (Ok nope) |}]
  end )
