(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

let print_result (type a) (pp_inner : a Fmt.t) : a Or_error.t -> unit =
  Fmt.(pr "@[%a@]@." (result ~ok:pp_inner ~error:Error.pp))

let%test_module "Backend" =
  ( module struct
    let machines = Lazy.force Data.Spec_sets.single_local_machine

    module Lookup = Act_machine.Lookup.Backend (struct
      (* TODO(@MattWindsor91): test this too. *)
      let test _ = Or_error.return ()
    end)

    let%test_module "single lookup" =
      ( module struct
        let test (fqid : Act_common.Id.t) : unit =
          let result = Lookup.lookup_single machines ~fqid in
          print_result
            (Fmt.using Act_machine.Qualified.spec
               Act_backend.Spec.With_id.pp)
            result

        let%expect_test "positive example" =
          test (Act_common.Id.of_string "localhost.herd") ;
          [%expect
            {|
            Enabled: true
            Style: herd
            Command: herd7 |}]

        let%expect_test "negative example" =
          test (Act_common.Id.of_string "localhost.litmus") ;
          [%expect
            {|
          ("unknown ID" (of_type backend) (id (localhost litmus))
           (suggestions ((localhost herd)))) |}]

        let%expect_test "negative example: wrong machine" =
          test (Act_common.Id.of_string "kappa.herd") ;
          [%expect
            {|
        ("unknown ID" (of_type backend) (id (kappa herd))
         (suggestions ((localhost herd)))) |}]
      end )
  end )
