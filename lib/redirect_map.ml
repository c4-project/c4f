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
include Redirect_map_intf

module Make (B : Basic_symbol) : S with type sym := B.t and type sym_set := B.Set.t =
struct
  type t = B.t B.Map.t [@@deriving sexp]

  let dest_of_sym (map : t) (sym : B.t) : B.t =
    Option.value (B.Map.find map sym) ~default:sym
  ;;

  let dest_of_id (map : t)
                 (id : C_identifier.t) : C_identifier.t Or_error.t =
    let open Or_error.Let_syntax in
    let%bind sym = B.of_c_identifier id in
    let redirected_sym = dest_of_sym map sym in
    B.to_c_identifier redirected_sym
  ;;

  let dest_syms (map : t) ~(sources : B.Set.t) : B.Set.t =
    (* We can't just take the image of 'sources' in 'map',
       as this would miss out identity mappings. *)
    sources |> B.Set.to_list |> List.map ~f:(dest_of_sym map) |> B.Set.of_list
  ;;

  let dest_ids (map : t) ~(sources : C_identifier.Set.t) : C_identifier.Set.t Or_error.t =
    let open Or_error.Let_syntax in
    let%bind source_sym_list =
      sources
      |> C_identifier.Set.to_list
      |> List.map ~f:B.of_c_identifier
      |> Or_error.combine_errors
    in
    let source_syms = B.Set.of_list source_sym_list in
    let dest_syms = dest_syms map ~sources:source_syms in
    let%map dest_id_list =
      dest_syms
      |> B.Set.to_list
      |> List.map ~f:B.to_c_identifier
      |> Or_error.combine_errors
    in
    C_identifier.Set.of_list dest_id_list
  ;;

  let of_symbol_alist : (B.t, B.t) List.Assoc.t -> t Or_error.t = B.Map.of_alist_or_error

  let to_string_alist (map : t) : (string, string) List.Assoc.t =
    map |> Map.to_alist |> Travesty.T_alist.bi_map ~left:B.to_string ~right:B.to_string
  ;;

  let check_no_tids (cvars : Config.C_variables.Map.t) : unit Or_error.t =
    let cvars_with_tids =
      cvars |> Map.filter ~f:Config.C_variables.Record.has_tid |> Map.keys
    in
    match cvars_with_tids with
    | [] -> Result.ok_unit
    | _ ->
      Or_error.error_s
        [%message
          "Expected a C variable map without thread IDs"
            ~cvars_with_tids:(cvars_with_tids : C_identifier.t list)]
  ;;

  let transform_c_variables (map : t)
                            (cvars : Config.C_variables.Map.t)
      : Config.C_variables.Map.t Or_error.t
    =
    let open Or_error.Let_syntax in
    let%bind () = check_no_tids cvars in
    let%bind alist =
      cvars
      |> Map.to_alist
      |> List.map ~f:(fun (var, record) -> var |> dest_of_id map >>| fun v' -> v', record)
      |> Or_error.combine_errors
    in
    C_identifier.Map.of_alist_or_error alist
  ;;

  let sources_of_sym (map : t) (dest : B.t) : B.Set.t =
    map |> B.Map.filter ~f:([%equal: B.t] dest) |> B.Map.keys |> B.Set.of_list
  ;;

  let propagate (src : B.t) (new_dst : B.t) (old_dst : B.t) : B.t =
    if [%equal: B.t] old_dst src then new_dst else old_dst
  ;;

  let redirect ~src ~dst map =
    let map' = B.Map.set map ~key:src ~data:dst in
    if [%equal: B.t] src dst then map' else B.Map.map map' ~f:(propagate src dst)
  ;;

  let identity () : t = B.Map.empty
end

let%test_module "string redirect maps" =
  (module struct
    module M = Make (struct
      include Core_kernel.String

      let to_string = Fn.id
      let of_string = Fn.id
      let of_c_identifier x = x |> C_identifier.to_string |> Or_error.return
      let to_c_identifier = C_identifier.create
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
    ;;

    let%expect_test "dest_syms example run" =
      let sources =
        Core_kernel.String.Set.of_list
          [ "alpha"; "bravo"; "charlie"; "%delta"; "_echo"; "hotel"; "whiskey" ]
      in
      let r = M.dest_syms test_map ~sources in
      Stdio.print_s [%sexp (r : Core_kernel.String.Set.t)];
      [%expect {| (%delta .foxtrot _hotel alpha whiskey) |}]
    ;;

    let%expect_test "dest_of_sym example run" =
      M.(
        let r =
          let alpha = dest_of_sym test_map "alpha" in
          let bravo = dest_of_sym test_map "bravo" in
          let charlie = dest_of_sym test_map "charlie" in
          let delta = dest_of_sym test_map "%delta" in
          alpha, bravo, charlie, delta
        in
        Stdio.print_s [%sexp (r : string * string * string * string)]);
      [%expect {| (alpha .foxtrot .foxtrot %delta) |}]
    ;;

    let%expect_test "String.R_map: sources_of example run" =
      M.(
        let r =
          let alpha = sources_of_sym test_map "alpha" in
          let bravo = sources_of_sym test_map "bravo" in
          let echo = sources_of_sym test_map "_echo" in
          let foxtrot = sources_of_sym test_map ".foxtrot" in
          alpha, bravo, echo, foxtrot
        in
        Stdio.print_s
          [%sexp
            (r
              : Core_kernel.String.Set.t
                * Core_kernel.String.Set.t
                * Core_kernel.String.Set.t
                * Core_kernel.String.Set.t)]);
      [%expect {| ((alpha) () () (_echo bravo charlie)) |}]
    ;;

    let%expect_test "dest_of_id: in map, valid ID" =
      let foo = C_identifier.of_string "hotel" in
      Stdio.print_s [%sexp (M.dest_of_id test_map foo : C_identifier.t Or_error.t)];
      [%expect {| (Ok _hotel) |}]
    ;;

    let%expect_test "dest_of_id: in map, invalid ID" =
      let foo = C_identifier.of_string "_echo" in
      Stdio.print_s [%sexp (M.dest_of_id test_map foo : C_identifier.t Or_error.t)];
      [%expect
        {|
        (Error
         ("validation failed"
          (.foxtrot
           ("validation errors" (("fst.char '.'" "Invalid initial character.")))
           utils/c_identifier.ml:55:13))) |}]
    ;;

    let%expect_test "dest_of_id: not in map" =
      let foo = C_identifier.of_string "nope" in
      Stdio.print_s [%sexp (M.dest_of_id test_map foo : C_identifier.t Or_error.t)];
      [%expect {| (Ok nope) |}]
    ;;

    let example_cvars_working : Config.C_variables.Map.t =
      Config.C_variables.Map.(
        merge_list
          [ of_single_scope_map
              ~scope:Config.C_variables.Scope.Local
              (C_identifier.Map.of_alist_exn
                 [ C_identifier.of_string "alpha", Some 27
                 ; C_identifier.of_string "hotel", None
                 ])
          ; of_single_scope_map
              ~scope:Config.C_variables.Scope.Global
              (C_identifier.Map.of_alist_exn [ C_identifier.of_string "BEEP", Some 53 ])
          ])
    ;;

    let%expect_test "transform_c_variables: working example" =
      Stdio.print_s
        [%sexp
          (M.transform_c_variables test_map example_cvars_working
            : Config.C_variables.Map.t Or_error.t)];
      [%expect
        {|
      (Ok
       ((BEEP ((scope Global) (initial_value (53)) (tid ())))
        (_hotel ((scope Local) (initial_value ()) (tid ())))
        (alpha ((scope Local) (initial_value (27)) (tid ()))))) |}]
    ;;
  end)
;;
