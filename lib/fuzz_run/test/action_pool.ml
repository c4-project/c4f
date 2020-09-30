(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

let%test_module "recommendation queue" =
  ( module struct
    (* TODO(@MattWindsor91): unify with whichever other test module it was
       that makes throwaway actions. *)
    let make_action (id : Common.Id.t) : Fuzz.Action.t =
      ( module struct
        let name = id

        let readme () = "dummy action"

        module Payload = Fuzz.Payload_impl.None

        let available = Fuzz.Availability.always

        let run test ~payload:() = Fuzz.State.Monad.return test
      end : Fuzz.Action_types.S )

    let action1_id = Common.Id.("test1" @: empty)

    let action2_id = Common.Id.("test2" @: empty)

    let action3_id = Common.Id.("test3" @: empty)

    let action1 = make_action action1_id

    let action2 = make_action action2_id

    let action3 = make_action action3_id

    let table =
      Fuzz.Action.With_default_weight.
        [ (action1 @-> 1, None)
        ; (action2 @-> 2, Some 0)
        ; (action3 @-> 0, None) ]

    let pool (queue_flag : Fuzz.Flag.t) : Src.Action_pool.t Or_error.t =
      Src.Action_pool.of_weighted_actions table ~queue_flag

    let pick_many (pool : Src.Action_pool.t) (n : int) :
        Common.Id.t list Or_error.t =
      let random =
        Splittable_random.State.of_int 0
        (* deterministic tests *)
      in
      Or_error.Let_syntax.(
        let%map _, xs =
          Travesty_base_exts.List.With_errors.fold_map_m
            (List.init n ~f:(fun _ -> ()))
            ~init:pool
            ~f:(fun pool () ->
              let%map (module A), pool' =
                Src.Action_pool.pick pool ~random
              in
              (pool', A.name))
        in
        xs)

    let print_result : Common.Id.t list Or_error.t -> unit =
      Fmt.(
        pr "@[%a@]@."
          (result ~error:Error.pp ~ok:(list ~sep:comma Common.Id.pp)))

    let test_choice (queue_flag : Fuzz.Flag.t) (n : int)
        (names : Common.Id.t list) : unit =
      print_result
        Or_error.(
          Let_syntax.(
            let%bind pool = pool queue_flag in
            let%bind pool' = Src.Action_pool.recommend pool ~names in
            pick_many pool' n))

    let%expect_test "choice when there is no recommendation" =
      test_choice (Fuzz.Flag.exact true) 1 [] ;
      [%expect {| test1 |}]

    let%expect_test "choice when there are recommendations, but \
                     recommending is off" =
      test_choice (Fuzz.Flag.exact false) 1 [] ;
      [%expect {| test1 |}]

    let%expect_test "choice when there are recommendations" =
      test_choice (Fuzz.Flag.exact true) 3 [action2_id; action3_id] ;
      [%expect {| test2, test3, test1 |}]

    let%expect_test "trying to take the same choice twice" =
      test_choice (Fuzz.Flag.exact true) 2 [] ;
      [%expect {| No choices have a positive weight |}]

    let%expect_test "trying to take the same choice through rec and deck" =
      test_choice (Fuzz.Flag.exact true) 2 [action1_id] ;
      [%expect {| No choices have a positive weight |}]

    let%expect_test "reset in between taking same choice" =
      let random =
        Splittable_random.State.of_int 0
        (* deterministic tests *)
      in
      print_result
        Or_error.(
          Let_syntax.(
            let%bind pool = pool (Fuzz.Flag.exact true) in
            let%bind (module A), pool = Src.Action_pool.pick pool ~random in
            let pool = Src.Action_pool.reset pool in
            let%map (module B), _ = Src.Action_pool.pick pool ~random in
            [A.name; B.name])) ;
      [%expect {| test1, test1 |}]

    let%expect_test "reset doesn't reset recommendations" =
      let random =
        Splittable_random.State.of_int 0
        (* deterministic tests *)
      in
      print_result
        Or_error.(
          Let_syntax.(
            let%bind pool = pool (Fuzz.Flag.exact true) in
            let%bind pool =
              Src.Action_pool.recommend pool ~names:[action2_id; action3_id]
            in
            let%bind (module A), pool = Src.Action_pool.pick pool ~random in
            let pool = Src.Action_pool.reset pool in
            let%bind (module B), pool = Src.Action_pool.pick pool ~random in
            let pool = Src.Action_pool.reset pool in
            let%map (module C), _ = Src.Action_pool.pick pool ~random in
            [A.name; B.name; C.name])) ;
      [%expect {| test2, test3, test1 |}]

    let%expect_test "reset in between taking choice as rec and taking from \
                     deck" =
      let random =
        Splittable_random.State.of_int 0
        (* deterministic tests *)
      in
      print_result
        Or_error.(
          Let_syntax.(
            let%bind pool = pool (Fuzz.Flag.exact true) in
            let%bind pool =
              Src.Action_pool.recommend pool ~names:[action1_id]
            in
            let%bind (module A), pool = Src.Action_pool.pick pool ~random in
            let pool = Src.Action_pool.reset pool in
            let%map (module B), _ = Src.Action_pool.pick pool ~random in
            [A.name; B.name])) ;
      [%expect {| test1, test1 |}]
  end )
