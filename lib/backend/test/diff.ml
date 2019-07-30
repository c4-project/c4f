(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Src = Act_backend
module Ac = Act_common
module Tx = Travesty_base_exts
module Ob = Src.Output.Observation

let%test_module "run" =
  ( module struct
    let sample_location_map_raw : (string, string option) List.Assoc.t =
      [ ("t0a", Some "0:a")
      ; ("t0b", Some "0:b")
      ; ("t1a", Some "1:a")
      ; ("t1b", Some "1:b")
      ; ("x", Some "x")
      ; ("y", Some "y")
      ; ("0:temp", None)
      ; ("1:temp", None) ]

    let location_map : Ac.Litmus_id.t option Map.M(Ac.Litmus_id).t =
      sample_location_map_raw
      |> Tx.Alist.bi_map ~left:Ac.Litmus_id.of_string
           ~right:(Option.map ~f:Ac.Litmus_id.of_string)
      |> Map.of_alist_exn (module Ac.Litmus_id)

    let add_state_exn (xs : (string, string) List.Assoc.t) (o : Ob.t) : Ob.t
        =
      Or_error.(
        xs
        |> Tx.Alist.map_left ~f:Ac.Litmus_id.of_string
        |> Src.State.of_alist
        >>= (fun state -> Ob.add o ~state)
        |> Or_error.ok_exn)

    let normal_oracle : Ob.t =
      Ob.(
        init ()
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
      let result =
        Src.Diff.run ~oracle:normal_oracle ~subject ~location_map
      in
      Fmt.(pr "@[%a@]@." (result ~ok:Src.Diff.pp ~error:Error.pp)) result

    let%expect_test "no subject states" =
      test (Ob.init ()) ;
      [%expect
        {|
      Oracle >> Subject
      In oracle only:
        {} |}]

    let smaller_subject : Ob.t =
      Ob.init ()
      |> add_state_exn
           [ ("t0a", "potato")
           ; ("t0b", "waffles")
           ; ("t1a", "true")
           ; ("t1b", "6")
           ; ("x", "100")
           ; ("y", "kg") ]
      |> add_state_exn
           [ ("t0a", "sweet")
           ; ("t0b", "waffles")
           ; ("t1a", "false")
           ; ("t1b", "42")
           ; ("x", "100")
           ; ("y", "cwt") ]

    let%expect_test "smaller subject states" =
      test smaller_subject ;
      [%expect
        {|
      Oracle >> Subject
      In oracle only:
        {x = 99; y = kg; 0:a = potato; 0:b = wedges; 1:a = true; 1:b = 42} |}]

    let equivalent_subject : Ob.t =
      smaller_subject
      |> add_state_exn
           [ ("t0a", "potato")
           ; ("t0b", "wedges")
           ; ("t1a", "true")
           ; ("t1b", "42")
           ; ("x", "99")
           ; ("y", "kg") ]

    let%expect_test "equivalent subject states" =
      test equivalent_subject ; [%expect {|
      Oracle == Subject |}]

    let larger_subject : Ob.t =
      equivalent_subject
      |> add_state_exn
           [ ("t0a", "potato")
           ; ("t0b", "heads")
           ; ("t1a", "true")
           ; ("t1b", "2")
           ; ("x", "99")
           ; ("y", "red balloons") ]

    let%expect_test "larger subject states" =
      test larger_subject ;
      [%expect
        {|
      Oracle << Subject
      In subject only:
        {x = 99; y = red balloons; 0:a = potato; 0:b = heads; 1:a = true; 1:b = 2} |}]

    let%expect_test "uncorrelated subject states" =
      test
        Ob.(
          init ()
          |> add_state_exn
               [ ("t0a", "potato")
               ; ("t0b", "waffles")
               ; ("t1a", "true")
               ; ("t1b", "6")
               ; ("x", "100")
               ; ("y", "kg") ]
          |> add_state_exn
               [ ("t0a", "sweet")
               ; ("t0b", "waffles")
               ; ("t1a", "false")
               ; ("t1b", "42")
               ; ("x", "100")
               ; ("y", "percent") ]
          |> add_state_exn
               [ ("t0a", "potato")
               ; ("t0b", "wedges")
               ; ("t1a", "true")
               ; ("t1b", "42")
               ; ("x", "99")
               ; ("y", "kg") ]) ;
      [%expect
        {|
      Oracle <> Subject
      In oracle only:
        {x = 100; y = cwt; 0:a = sweet; 0:b = waffles; 1:a = false; 1:b = 42}
      In subject only:
        {x = 100; y = percent; 0:a = sweet; 0:b = waffles; 1:a = false; 1:b = 42} |}]
  end )
