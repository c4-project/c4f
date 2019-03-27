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

include Core
include Utils
include Language_symbol_intf

let program_id_of_demangled sym =
  let open Option.Let_syntax in
  let%bind num_s = String.chop_prefix ~prefix:"P" sym in
  let%bind num = Caml.int_of_string_opt num_s in
  Option.some_if (Int.is_non_negative num) num
;;

let%expect_test "program_id_of_demangled: valid" =
  Sexp.output Out_channel.stdout [%sexp (program_id_of_demangled "P0" : int option)];
  [%expect {| (0) |}]
;;

module Make (B : Basic) : S with type t = B.t = struct
  include B

  let of_string_opt (s : string) = Result.ok (B.require_of_string s)

  module Comp = Comparable.Make (B)

  module Set = struct
    include My_set.Extend (Comp.Set)

    let abstract = Abstract.Symbol.Set.map ~f:B.abstract
  end

  include (Comp : Comparable.S with type t := t
                        and type comparator_witness = Set.Elt.comparator_witness
                        and module Set := Set)


  module R_map = struct
    module M = Comp.Map

    type sym = t [@@deriving compare, equal, sexp]

    type r_dest =
      | Identity
      | MapsTo of sym
    [@@deriving sexp, equal]

    type t = r_dest M.t [@@deriving sexp]

    module Set = Set

    let make : Set.t -> t = Set.to_map ~f:(Fn.const Identity)

    let sources_of (rmap : t) d =
      let indirects =
        rmap
        |> M.filter ~f:(equal_r_dest (MapsTo d))
        |> M.to_sequence
        |> Sequence.map ~f:fst
        |> Sequence.to_list
      in
      let directs =
        match M.find rmap d with
        | Some Identity -> [ d ]
        | Some _ | None -> []
      in
      directs @ indirects
    ;;

    let dest_of = M.find

    let to_dest = function
      | src, Identity -> src
      | _, MapsTo x -> x
    ;;

    let all_dests rmap = rmap |> M.to_alist |> List.map ~f:to_dest |> Set.of_list

    let add_maps_to_edge ~src ~dst rmap =
      rmap
      |> M.set ~key:src ~data:(MapsTo dst)
      |> M.mapi ~f:(fun ~key ~data ->
             match data with
             | MapsTo dst' when equal_sym src dst' ->
               if equal_sym key dst then Identity else MapsTo dst
             | x -> x)
    ;;

    let redirect ~src ~dst rmap =
      Or_error.(
        if equal_sym src dst
        then return (M.set rmap ~key:src ~data:Identity)
        else return (add_maps_to_edge ~src ~dst rmap))
    ;;
  end

  let program_id_of sym =
    let asyms = B.abstract_demangle sym in
    let ids = List.filter_map asyms ~f:program_id_of_demangled in
    (* The demangled symbols should be in order of likelihood, so we
       take a gamble on the first one being the most likely program ID
       in case of a tie. *)
    List.hd ids
  ;;

  let is_c_safe (sym : t) : bool = Utils.C_identifier.is_string_safe (to_string sym)

  let is_herd_safe (sym : t) : bool =
    Utils.C_identifier.Herd_safe.is_string_safe (to_string sym)
  ;;

  let is_program_label sym = Option.is_some (program_id_of sym)
end

module String_direct : S with type t = string = Make (struct
  include String

  let abstract = Fn.id
  let abstract_demangle = List.return
  let require_of_string = Or_error.return

  module On_strings = struct
    type t = string

    include Travesty.Singleton.With_elt (String)
  end
end)

let string_test_rmap : String_direct.R_map.t Or_error.t Lazy.t =
  lazy
    Or_error.(
      String_direct.R_map.(
        return (make (String_direct.Set.singleton "whiskey"))
        >>= redirect ~src:"alpha" ~dst:"alpha"
        >>= redirect ~src:"bravo" ~dst:"_echo"
        >>= redirect ~src:"charlie" ~dst:"_echo"
        >>= redirect ~src:"_echo" ~dst:".foxtrot"
        >>= redirect ~src:"%delta" ~dst:"$kilo"
        >>= redirect ~src:"$kilo" ~dst:"%delta"))
;;

let%test_module "String R-maps" =
  (module struct
     let test_rmap = Lazy.force string_test_rmap

     let%expect_test "String.R_map: all_dests example run" =
       let open Or_error.Let_syntax in
       String_direct.R_map.(
         let r =
           let%map map = test_rmap in
           all_dests map
         in
         Sexp.output_hum
           Out_channel.stdout
           [%sexp (r : String_direct.R_map.Set.t Or_error.t)]);
       [%expect {| (Ok (%delta .foxtrot alpha whiskey)) |}]
     ;;

     let%expect_test "String.R_map: dest_of example run" =
       let open Or_error.Let_syntax in
       String_direct.R_map.(
         let r =
           let%map map = test_rmap in
           let alpha = dest_of map "alpha" in
           let bravo = dest_of map "bravo" in
           let charlie = dest_of map "charlie" in
           let delta = dest_of map "%delta" in
           alpha, bravo, charlie, delta
         in
         Sexp.output_hum
           Out_channel.stdout
           [%sexp
             (r
               : (r_dest option * r_dest option * r_dest option * r_dest option)
                 Or_error.t)]);
       [%expect
         {| (Ok ((Identity) ((MapsTo .foxtrot)) ((MapsTo .foxtrot)) (Identity))) |}]
     ;;

     let%expect_test "String.R_map: sources_of example run" =
       let open Or_error.Let_syntax in
       String_direct.R_map.(
         let r =
           let%map map = test_rmap in
           let alpha = sources_of map "alpha" in
           let bravo = sources_of map "bravo" in
           let echo = sources_of map "_echo" in
           let foxtrot = sources_of map ".foxtrot" in
           alpha, bravo, echo, foxtrot
         in
         Sexp.output_hum
           Out_channel.stdout
           [%sexp
             (r : (string list * string list * string list * string list) Or_error.t)]);
       [%expect {| (Ok ((alpha) () () (_echo bravo charlie))) |}]
     ;;
  end)
;;
