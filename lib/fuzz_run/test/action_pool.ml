(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
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

        let readme = lazy "dummy action"

        let recommendations () = []

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

    let pool ~(acc : Fuzz.Flag.t) ~(use : Fuzz.Flag.t) :
        Src.Action_pool.t Or_error.t =
      Src.Action_pool.of_weighted_actions table ~accept_rec_flag:acc
        ~use_rec_flag:use

    let print_result : Fuzz.Action.t list Or_error.t -> unit =
      Fmt.(
        pr "@[%a@]@."
          (result ~error:Error.pp
             ~ok:
               (list ~sep:comma
                  (using
                     (fun (module A : Fuzz.Action_types.S) -> A.name)
                     Common.Id.pp ) ) ) )

    let test_choice ~(acc : Fuzz.Flag.t) ~(use : Fuzz.Flag.t) (max : int)
        (names : Common.Id.t list) : unit =
      let random =
        Splittable_random.State.of_int 0
        (* deterministic tests *)
      in
      print_result
        Or_error.(
          pool ~acc ~use
          >>| Src.Action_pool.recommend ~names ~random
          >>= Src.Action_pool.pick_many ~max ~random
          >>| fst )

    let%expect_test "choice when there is no recommendation" =
      test_choice ~acc:(Fuzz.Flag.exact true) ~use:(Fuzz.Flag.exact true) 1
        [] ;
      [%expect {| test1 |}]

    let%expect_test "choice when there are recommendations, but acceptance \
                     is off" =
      test_choice ~acc:(Fuzz.Flag.exact false) ~use:(Fuzz.Flag.exact true) 1
        [action2_id; action3_id] ;
      [%expect {| test1 |}]

    let%expect_test "choice when there are recommendations, but \
                     recommending is off" =
      test_choice ~acc:(Fuzz.Flag.exact true) ~use:(Fuzz.Flag.exact false) 1
        [action2_id; action3_id] ;
      [%expect {| test1 |}]

    let%expect_test "choice when there are recommendations, and we always \
                     accept and use them" =
      test_choice ~acc:(Fuzz.Flag.exact true) ~use:(Fuzz.Flag.exact true) 3
        [action2_id; action3_id] ;
      [%expect {| test2, test3, test1 |}]

    let%expect_test "choice when there are recommendations, and we \
                     sometimes don't accept them" =
      test_choice ~use:(Fuzz.Flag.exact true)
        ~acc:(Or_error.ok_exn (Fuzz.Flag.try_make ~wins:1 ~losses:1))
        3 [action2_id; action3_id] ;
      [%expect {| test2, test1 |}]

    let%expect_test "choice when there are recommendations, and we \
                     sometimes don't use them" =
      test_choice ~acc:(Fuzz.Flag.exact true)
        ~use:(Or_error.ok_exn (Fuzz.Flag.try_make ~wins:1 ~losses:1))
        3 [action2_id; action3_id] ;
      [%expect {| test2, test1, test3 |}]

    let%expect_test "trying to take the same choice twice" =
      test_choice ~acc:(Fuzz.Flag.exact true) ~use:(Fuzz.Flag.exact true) 2
        [] ;
      [%expect {| test1 |}]

    let%expect_test "trying to take the same choice through rec and deck" =
      test_choice ~acc:(Fuzz.Flag.exact true) ~use:(Fuzz.Flag.exact true) 2
        [action1_id] ;
      [%expect {| test1 |}]

    let%expect_test "reset in between taking same choice" =
      let random =
        Splittable_random.State.of_int 0
        (* deterministic tests *)
      in
      print_result
        Or_error.(
          Let_syntax.(
            let%bind pool =
              pool ~acc:(Fuzz.Flag.exact true) ~use:(Fuzz.Flag.exact true)
            in
            let%bind a, pool = Src.Action_pool.pick pool ~random in
            let pool = Src.Action_pool.reset pool in
            let%map b, _ = Src.Action_pool.pick pool ~random in
            List.filter_opt [a; b] ) ) ;
      [%expect {| test1, test1 |}]

    let%expect_test "reset doesn't reset recommendations" =
      let random =
        Splittable_random.State.of_int 0
        (* deterministic tests *)
      in
      print_result
        Or_error.(
          Let_syntax.(
            let%bind pool =
              pool ~acc:(Fuzz.Flag.exact true) ~use:(Fuzz.Flag.exact true)
            in
            let pool =
              Src.Action_pool.recommend pool ~names:[action2_id; action3_id]
                ~random
            in
            let%bind a, pool = Src.Action_pool.pick pool ~random in
            let pool = Src.Action_pool.reset pool in
            let%bind b, pool = Src.Action_pool.pick pool ~random in
            let pool = Src.Action_pool.reset pool in
            let%map c, _ = Src.Action_pool.pick pool ~random in
            List.filter_opt [a; b; c] ) ) ;
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
            let%bind pool =
              pool ~acc:(Fuzz.Flag.exact true) ~use:(Fuzz.Flag.exact true)
            in
            let pool =
              Src.Action_pool.recommend pool ~names:[action1_id] ~random
            in
            let%bind a, pool = Src.Action_pool.pick pool ~random in
            let pool = Src.Action_pool.reset pool in
            let%map b, _ = Src.Action_pool.pick pool ~random in
            List.filter_opt [a; b] ) ) ;
      [%expect {| test1, test1 |}]
  end )
