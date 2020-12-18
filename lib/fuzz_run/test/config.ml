(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

let%test_module "Pool making" =
  ( module struct
    let test (flag : Fuzz.Flag.t) : unit =
      (* Basically, print a sample of 10 actions from the generated pool,
         with a deterministic randomiser.

         This test will change every time the action list changes, but we
         already have tests that do that, so _shrug_ *)
      let random = Splittable_random.State.of_int 0 in
      let flags =
        Map.of_alist_exn
          (module Common.Id)
          [(Fuzz.Config_tables.action_enable_flag, flag)]
      in
      let conf = Src.Config.make ~flags () in
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

    let%expect_test "always pick" =
      test (Fuzz.Flag.exact true) ;
      [%expect
        {|
        dead.insert.early-out-loop-end, program.label,
        atomic.store.insert.int.normal, if.transform.invert, if.surround.tautology,
        var.make, if.surround.duplicate, loop.surround.while.dead, nop, mem.fence |}]

    let%expect_test "never pick" =
      test (Fuzz.Flag.exact false) ;
      [%expect {|
        nop |}]

    let%expect_test "pick half the time" =
      test (Or_error.ok_exn (Fuzz.Flag.try_make ~wins:1 ~losses:1)) ;
      [%expect
        {|
        var.make, mem.strengthen, atomic.store.insert.int.normal,
        var.assign.insert.int.redundant, atomic.fetch.insert.cond.boundary,
        mem.fence, loop.surround.do.dead, loop.insert.for.kv-never,
        loop.surround.while.dead, atomic.cmpxchg.insert.int.succeed |}]

    let%expect_test "pick almost never" =
      test (Or_error.ok_exn (Fuzz.Flag.try_make ~wins:1 ~losses:10000000)) ;
      [%expect {|
        nop |}]
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
