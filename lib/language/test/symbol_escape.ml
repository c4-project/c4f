(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Stdio
module Tx = Travesty_base_exts
open Act_language.Symbol_escape

let%expect_test "escape_string: sample" =
  print_string (escape_string "_foo$bar.BAZ@lorem-ipsum+dolor,sit%amet") ;
  [%expect {| ZUfooZDbarZFBAZZZTloremZMipsumZAdolorZCsitZPamet |}]

let%test_module "escaping on a toy symbol module" =
  ( module struct
    module M = Make (Act_language.Symbol.String_direct)

    let%expect_test "escape: sample" =
      Stdio.print_string (M.escape "_example_two%100.5@@etc,etc,etc$6") ;
      [%expect {| ZUexampleZUtwoZP100ZF5ZTZTetcZCetcZCetcZD6 |}]

    let%test_unit "escape: should be injective" =
      let open Base_quickcheck in
      Test.run_exn
        ( module struct
          type t = string * string [@@deriving sexp]

          let quickcheck_generator =
            Base_quickcheck.Generator.filter
              [%quickcheck.generator: string * string] ~f:(fun (x, y) ->
                not (String.equal x y))

          let quickcheck_shrinker = [%quickcheck.shrinker: string * string]
        end )
        ~f:
          ([%test_pred: string * string] ~here:[[%here]] (fun (x, y) ->
               not (Tx.Fn.on M.escape ~f:String.equal x y)))

    (* TODO(@MattWindsor91): fix this test let%expect_test "escape_rmap:
       sample" = let test_map =

       let result = Or_error.(Language_symbol.string_test_rmap |> Lazy.force
       >>= M.escape_rmap) in Stdio.print_s [%sexp (result :
       Language_symbol.String_direct.R_map.t Or_error.t)]; [%expect {| (Ok
       (($kilo (MapsTo ZPdelta)) (%delta (MapsTo ZPdelta)) (.foxtrot (MapsTo
       ZFfoxtrot)) (_echo (MapsTo ZFfoxtrot)) (alpha Identity) (bravo (MapsTo
       ZFfoxtrot)) (charlie (MapsTo ZFfoxtrot)) (whiskey Identity))) |}] ;; *)
  end )
