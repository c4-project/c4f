(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Base_quickcheck
open Stdio
open Act_fuzz
open Act_fuzz.Trace

let%test_module "S-expression representation" =
  ( module struct
    module Dummy_payload = struct
      type t = {foo: int; bar: bool; baz: string}
      [@@deriving sexp, quickcheck]
    end

    module Dummy_action :
      Action_types.S with type Payload.t = Dummy_payload.t = struct
      let name = Act_common.Id.of_string "dummy.action"

      let available = Action.always

      let default_weight = 1

      let readme () =
        {| This is a module that is almost, but not quite, entirely unlike a fuzzer action. |}

      module Payload = Action.Pure_payload (Dummy_payload)

      let run subject ~(payload : Dummy_payload.t) = ignore payload; State.Monad.return subject
    end

    module Another_dummy_action :
      Action_types.S with type Payload.t = unit = struct
      let name = Act_common.Id.of_string "another.dummy.action"

      let available = Action.always

      let default_weight = 1

      let readme () =
        {| This is also a module that is almost, but not quite, entirely unlike a fuzzer action. |}

      module Payload = Action.No_payload

      let run subject ~(payload : unit) = ignore payload; State.Monad.return subject
    end

    let%expect_test "empty trace" =
      print_s [%sexp (empty : t)] ;
      [%expect {| () |}]

    let%expect_test "example trace" =
      let trace =
        empty
        |> add
             ~action:(module Dummy_action)
             ~payload:{foo= 27; bar= true; baz= "hello"}
        |> add ~action:(module Another_dummy_action) ~payload:()
        |> add
             ~action:(module Dummy_action)
             ~payload:{foo= 53; bar= false; baz= "world"}
      in
      print_s [%sexp (trace : t)] ;
      [%expect
        {|
      (((name (dummy action)) (payload ((foo 27) (bar true) (baz hello))))
       ((name (another dummy action)) (payload ()))
       ((name (dummy action)) (payload ((foo 53) (bar false) (baz world))))) |}]
  end )
