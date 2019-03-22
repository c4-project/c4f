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

module Make (B : Basic_symbol) : S with type sym := B.t = struct
  type t = (B.t, B.t) List.Assoc.t [@@deriving sexp]

  let resolve_sym (map : t)
                  (symbol : B.t) : B.t Or_error.t =
    symbol
    |> List.Assoc.find map ~equal:[%equal: B.t]
    |> Result.of_option
         ~error:
           (Error.create_s
              [%message
                "Couldn't resolve symbol in redirects table"
                  ~here:[%here]
                  ~symbol:(symbol : B.t)
                  ~redirects:(map : t)])
  ;;

  let resolve_id (map : t)
                 (id : C_identifier.t) : C_identifier.t Or_error.t =
    let open Or_error.Let_syntax in
    let%bind sym = B.of_c_identifier id in
    let%bind redirected_sym = resolve_sym map sym in
    B.to_c_identifier redirected_sym
  ;;

  let image_ids (map : t) : C_identifier.Set.t Or_error.t =
    let open Or_error.Monad_infix in
    map
    |> List.map ~f:(fun (_, sym) -> B.to_c_identifier sym)
    |> Or_error.combine_errors
    >>| C_identifier.Set.of_list
  ;;

  let of_symbol_alist : (B.t, B.t) List.Assoc.t -> t = Fn.id

  let to_string_alist (map : t) : (string, string) List.Assoc.t =
    Travesty.T_alist.bi_map map ~left:B.to_string ~right:B.to_string
  ;;

  let check_no_tids (cvars : Config.C_variables.Map.t) : unit Or_error.t =
    let cvars_with_tids =
      cvars
      |> Map.filter ~f:(Config.C_variables.Record.has_tid)
      |> Map.keys
    in
    match cvars_with_tids with
    | [] -> Result.ok_unit
    | _ -> Or_error.error_s
             [%message "Expected a C variable map without thread IDs"
                 ~cvars_with_tids:(cvars_with_tids : C_identifier.t list)]
  ;;

  let transform_c_variables
    (map : t)
    (cvars : Config.C_variables.Map.t)
    : Config.C_variables.Map.t Or_error.t =
    let open Or_error.Let_syntax in
    let%bind () = check_no_tids cvars in
    let%bind alist =
      cvars
      |> Map.to_alist
      |> List.map ~f:(fun (var, record) ->
          var |> resolve_id map >>| fun v' -> (v', record)
        )
      |> Or_error.combine_errors
    in
    C_identifier.Map.of_alist_or_error alist
end

module Make_from_language_symbol (LS : Language_symbol.S) : S with type sym := LS.t =
Make (struct
  include LS

  let of_string (x : string) = Option.value_exn (LS.of_string_opt x)

  let to_c_identifier (s : LS.t) : C_identifier.t Or_error.t =
    s |> LS.to_string |> C_identifier.create
  ;;

  let of_c_identifier (id : C_identifier.t) : LS.t Or_error.t =
    id
    |> C_identifier.to_string
    |> LS.of_string_opt
    |> Result.of_option
         ~error:
           (Error.create_s
              [%message
                "Couldn't convert identifier to symbol"
                  ~here:[%here]
                  ~id:(id : C_identifier.t)])
  ;;
end)

let%test_module "string redirect maps" = (module struct
  module M = Make (struct
      type t = string [@@deriving equal, sexp]
      let to_string = Fn.id
      let of_string = Fn.id

      let of_c_identifier x = x |> C_identifier.to_string |> Or_error.return
      let to_c_identifier = C_identifier.create
    end)

  let example_map : M.t =
    M.of_symbol_alist
      [ ("foo", "_foo")
      ; ("bar_baz", "_bar_baz")
      ; ("BEEP", "_beep")
      ; ("_boop", "_2boop") (* why not? *)
      ]
  ;;

  let%expect_test "resolve_id: in map" =
    let foo = C_identifier.of_string "foo" in
    Stdio.print_s [%sexp (M.resolve_id example_map foo : C_identifier.t Or_error.t)];
    [%expect {| (Ok _foo) |}]
  ;;

  let%expect_test "resolve_id: not in map" =
    let foo = C_identifier.of_string "nope" in
    Stdio.print_s [%sexp (M.resolve_id example_map foo : C_identifier.t Or_error.t)];
    [%expect {|
      (Error
       ("Couldn't resolve symbol in redirects table"
        (here lib/redirect_map.ml:41:24) (symbol nope)
        (redirects ((foo _foo) (bar_baz _bar_baz) (BEEP _beep) (_boop _2boop))))) |}]
  ;;

  let example_cvars_working : Config.C_variables.Map.t =
    Config.C_variables.Map.(
      merge_list
        [ of_single_scope_map ~scope:Config.C_variables.Scope.Local
            (C_identifier.Map.of_alist_exn
               [ (C_identifier.of_string "foo", Some 27)
               ; (C_identifier.of_string "bar_baz", None)
               ]
            )
        ; of_single_scope_map ~scope:Config.C_variables.Scope.Global
            (C_identifier.Map.of_alist_exn
               [ (C_identifier.of_string "BEEP", Some 53)
               ]
            )
        ]
    )
  ;;

  let%expect_test "transform_c_variables: working example" =
    Stdio.print_s
      [%sexp (M.transform_c_variables example_map example_cvars_working
               : Config.C_variables.Map.t Or_error.t)];
    [%expect {|
      (Ok
       ((_bar_baz ((scope Local) (initial_value ()) (tid ())))
        (_beep ((scope Global) (initial_value (53)) (tid ())))
        (_foo ((scope Local) (initial_value (27)) (tid ()))))) |}]
  ;;
end)
