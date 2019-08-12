(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Src = Act_state
module Ac = Act_common
module Tx = Travesty_base_exts
module Ob = Src.Observation

let%test_module "run" =
  ( module struct
    let add_state_exn (xs : (string, string) List.Assoc.t) (o : Ob.t) : Ob.t
        =
      Or_error.(
        xs
        |> Tx.Alist.map_left ~f:Ac.Litmus_id.of_string
        |> Src.Entry.of_alist
        >>= (fun state -> Ob.add o ~state)
        |> Or_error.ok_exn)

    let normal_oracle : Ob.t =
      Ob.(
        empty
        |> add_state_exn
             [ ("0:a", "potato")
             ; ("0:b", "waffles")
             ; ("1:a", "true")
             ; ("1:b", "6")
             ; ("x", "100")
             ; ("y", "kg") ]
        |> add_state_exn
             [ ("0:a", "sweet")
             ; ("0:b", "waffles")
             ; ("1:a", "false")
             ; ("1:b", "42")
             ; ("x", "100")
             ; ("y", "cwt") ]
        |> add_state_exn
             [ ("0:a", "potato")
             ; ("0:b", "wedges")
             ; ("1:a", "true")
             ; ("1:b", "42")
             ; ("x", "99")
             ; ("y", "kg") ])

    let test (subject : Ob.t) : unit =
      let result = Src.Compare.run ~oracle:normal_oracle ~subject in
      Fmt.(pr "@[%a@]@." (result ~ok:Src.Compare.Result.pp ~error:Error.pp))
        result

    let%expect_test "no subject states" =
      test Ob.empty ;
      [%expect
        {|
      Oracle >> Subject
      In oracle only:
        {{}}
      Variables considered:
        {} |}]

    let smaller_subject : Ob.t =
      Ob.empty
      |> add_state_exn
           [ ("0:a", "potato")
           ; ("0:b", "waffles")
           ; ("1:a", "true")
           ; ("1:b", "6")
           ; ("x", "100")
           ; ("y", "kg") ]
      |> add_state_exn
           [ ("0:a", "sweet")
           ; ("0:b", "waffles")
           ; ("1:a", "false")
           ; ("1:b", "42")
           ; ("x", "100")
           ; ("y", "cwt") ]

    let%expect_test "smaller subject states" =
      test smaller_subject ;
      [%expect
        {|
      Oracle >> Subject
      In oracle only:
        {{x = 99; y = kg; 0:a = potato; 0:b = wedges; 1:a = true; 1:b = 42}}
      Variables considered:
        {x, y, 0:a, 0:b, 1:a, 1:b} |}]

    let equivalent_subject : Ob.t =
      smaller_subject
      |> add_state_exn
           [ ("0:a", "potato")
           ; ("0:b", "wedges")
           ; ("1:a", "true")
           ; ("1:b", "42")
           ; ("x", "99")
           ; ("y", "kg") ]

    let%expect_test "equivalent subject states" =
      test equivalent_subject ;
      [%expect
        {|
      Oracle == Subject
      Variables considered:
        {x, y, 0:a, 0:b, 1:a, 1:b} |}]

    let equivalent_subject : Ob.t =
      smaller_subject
      |> add_state_exn
           [ ("0:a", "potato")
           ; ("0:b", "wedges")
           ; ("1:a", "true")
           ; ("1:b", "42")
           ; ("x", "99")
           ; ("y", "kg") ]

    let%expect_test "equivalent subject states" =
      test equivalent_subject ;
      [%expect
        {|
      Oracle == Subject
      Variables considered:
        {x, y, 0:a, 0:b, 1:a, 1:b} |}]

    let larger_subject : Ob.t =
      equivalent_subject
      |> add_state_exn
           [ ("0:a", "potato")
           ; ("0:b", "heads")
           ; ("1:a", "true")
           ; ("1:b", "2")
           ; ("x", "99")
           ; ("y", "red balloons") ]

    let%expect_test "larger subject states" =
      test larger_subject ;
      [%expect
        {|
      Oracle << Subject
      In subject only:
        {{x = 99; y = red balloons; 0:a = potato; 0:b = heads; 1:a = true; 1:b = 2}}
      Variables considered:
        {x, y, 0:a, 0:b, 1:a, 1:b} |}]

    let%expect_test "uncorrelated subject states" =
      test
        Ob.(
          empty
          |> add_state_exn
               [ ("0:a", "potato")
               ; ("0:b", "waffles")
               ; ("1:a", "true")
               ; ("1:b", "6")
               ; ("x", "100")
               ; ("y", "kg") ]
          |> add_state_exn
               [ ("0:a", "sweet")
               ; ("0:b", "waffles")
               ; ("1:a", "false")
               ; ("1:b", "42")
               ; ("x", "100")
               ; ("y", "percent") ]
          |> add_state_exn
               [ ("0:a", "potato")
               ; ("0:b", "wedges")
               ; ("1:a", "true")
               ; ("1:b", "42")
               ; ("x", "99")
               ; ("y", "kg") ]) ;
      [%expect
        {|
      Oracle <> Subject
      In oracle only:
        {{x = 100; y = cwt; 0:a = sweet; 0:b = waffles; 1:a = false; 1:b = 42}}
      In subject only:
        {{x = 100; y = percent; 0:a = sweet; 0:b = waffles; 1:a = false; 1:b = 42}}
      Variables considered:
        {x, y, 0:a, 0:b, 1:a, 1:b} |}]
  end )