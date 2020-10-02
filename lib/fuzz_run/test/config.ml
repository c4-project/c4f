(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

let%test_module "Pool making" =
  ( module struct
    let test (size : int) : unit =
      (* Basically, print a sample of 10 actions from the generated pool,
         with a deterministic randomiser.

         This test will change every time the action list changes, but we
         already have tests that do that, so _shrug_ *)
      let random = Splittable_random.State.of_int 0 in
      let params =
        Map.of_alist_exn
          (module Common.Id)
          Fuzz.[(Config_tables.action_deck_size_param, size)]
      in
      let conf = Src.Config.make ~params () in
      let pmap = Src.Config.make_param_map conf in
      let result =
        Or_error.(
          Src.Config.make_pool conf pmap ~random
          >>= Src.Action_pool.pick_many ~max:10 ~random
          >>| fst)
      in
      Utils.My_format.fdump Stdio.stdout
        Fmt.(
          result ~error:Error.pp
            ~ok:
              (list ~sep:comma
                 (using
                    (fun (module A : Fuzz.Action_types.S) -> A.name)
                    Common.Id.pp)))
        result

    let%expect_test "no cap" =
      test (-1) ;
      [%expect
        {|
        loop.surround.do.false, var.assign.insert.int.redundant,
        dead.insert.early-out-loop-end, atomic.store.insert.int.normal,
        loop.insert.while.false, atomic.cmpxchg.insert.int.succeed,
        atomic.store.insert.int.redundant, var.make, var.assign.insert.int.dead,
        atomic.store.insert.int.dead |}]

    let%expect_test "zero cap" =
      test 0 ;
      [%expect
        {|
        loop.surround.do.false, var.assign.insert.int.redundant,
        dead.insert.early-out-loop-end, atomic.store.insert.int.normal,
        loop.insert.while.false, atomic.cmpxchg.insert.int.succeed,
        atomic.store.insert.int.redundant, var.make, var.assign.insert.int.dead,
        atomic.store.insert.int.dead |}]

    let%expect_test "one cap" =
      test 1 ; [%expect {|
        dead.insert.goto |}]

    let%expect_test "cap higher than action list length" =
      test 32767 ;
      [%expect
        {|
        var.assign.insert.int.redundant, dead.insert.goto, if.surround.tautology,
        atomic.fetch.insert.int.redundant, atomic.store.transform.xchgify,
        var.assign.insert.int.dead, var.volatile, mem.strengthen,
        if.surround.duplicate, loop.insert.while.false |}]

    let%expect_test "representative cap" =
      test 5 ;
      [%expect
        {|
        dead.insert.goto, atomic.store.transform.xchgify, dead.insert.early-out,
        var.make |}]
  end )

let%test_module "Weight summaries" =
  ( module struct
    let%expect_test "pp: example summary" =
      let value =
        Src.Summary.Adjusted.(Adjusted {original= 27; actual= 53})
      in
      let readme =
        {|
        Why, hello there!  This is an example README.

        It is very long...

        ...and has line breaks and other such interesting things in it.
        Hopefully, it'll be enough to be able to test that the action
        summary pretty-printer does what it's supposed to.

        Be seeing you.
      |}
      in
      let map =
        Map.singleton
          (module Common.Id)
          (Common.Id.of_string "foo")
          {Src.Summary.value; readme}
      in
      Fmt.pr "%a@." Src.Config.Weight_summary.pp map ;
      [%expect
        {|
      foo:
        Weight: 53x (normally 27x)
        Summary:
          Why, hello there! This is an example README.

          It is very long...

          ...and has line breaks and other such interesting things in it.
          Hopefully, it'll be enough to be able to test that the action summary
          pretty-printer does what it's supposed to.

          Be seeing you. |}]
  end )
