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
module Src = Act_fuzz

let%test_module "S-expression representation" =
  ( module struct
    module Dummy_payload = struct
      type t = {foo: int; bar: bool; baz: string}
      [@@deriving sexp, quickcheck]
    end

    module Dummy_action :
      Src.Action_types.S with type Payload.t = Dummy_payload.t = struct
      let name = Act_common.Id.of_string "dummy.action"

      let available = Src.Action.always

      let readme () =
        {| This is a module that is almost, but not quite, entirely unlike a fuzzer action. |}

      module Payload = Src.Payload.Pure (Dummy_payload)

      let run subject ~(payload : Dummy_payload.t) =
        ignore payload ;
        Src.State.Monad.return subject
    end

    module Another_dummy_action :
      Src.Action_types.S with type Payload.t = unit = struct
      let name = Act_common.Id.of_string "another.dummy.action"

      let available = Src.Action.always

      let readme () =
        {| This is also a module that is almost, but not quite, entirely unlike a fuzzer action. |}

      module Payload = Src.Payload.None

      let run subject ~(payload : unit) =
        ignore payload ;
        Src.State.Monad.return subject
    end

    let%expect_test "empty trace" =
      print_s [%sexp (Src.Trace.empty : Src.Trace.t)] ;
      [%expect {| () |}]

    let%expect_test "example trace" =
      let trace =
        Src.Trace.(
          empty
          |> add
               ~action:(module Dummy_action)
               ~payload:{foo= 27; bar= true; baz= "hello"}
          |> add ~action:(module Another_dummy_action) ~payload:()
          |> add
               ~action:(module Dummy_action)
               ~payload:{foo= 53; bar= false; baz= "world"})
      in
      print_s [%sexp (trace : Src.Trace.t)] ;
      [%expect
        {|
      (((name (dummy action)) (payload ((foo 27) (bar true) (baz hello))))
       ((name (another dummy action)) (payload ()))
       ((name (dummy action)) (payload ((foo 53) (bar false) (baz world))))) |}]
  end )

let%test_module "trace playback" =
  ( module struct
    let%expect_test "empty trace does nothing" =
      let test = Lazy.force Subject.Test_data.test in
      let state = Lazy.force Subject.Test_data.state in
      let computation =
        Src.Trace.(
          run empty test ~resolve:(fun _ ->
              Or_error.unimplemented "shouldn't run this"))
      in
      let result = Src.State.Monad.run computation state in
      print_s [%sexp (result : Src.Subject.Test.t Or_error.t)] ;
      [%expect
        {|
      (Ok
       ((header
         ((locations ()) (init ((x (Int 27)) (y (Int 53)))) (postcondition ())
          (name example)))
        (threads
         (((decls ())
           (stms
            ((Prim ((source Generated) (liveness Normal))
              (Atomic
               (Atomic_store
                ((src (Constant (Int 42))) (dst (Lvalue (Variable x)))
                 (mo memory_order_seq_cst)))))
             (Prim ((source Generated) (liveness Normal)) Nop)
             (Prim ((source Generated) (liveness Normal))
              (Atomic
               (Atomic_store
                ((src (Address (Lvalue (Variable foo))))
                 (dst (Lvalue (Variable y))) (mo memory_order_relaxed)))))
             (If_stm
              ((cond
                (Bop Eq (Address (Lvalue (Variable foo)))
                 (Address (Lvalue (Variable y)))))
               (t_branch
                ((statements
                  ((Prim ((source Generated) (liveness Normal))
                    (Atomic
                     (Atomic_store
                      ((src (Constant (Int 56))) (dst (Lvalue (Variable x)))
                       (mo memory_order_seq_cst)))))
                   (Prim ((source Generated) (liveness Normal))
                    (Label kappa_kappa))))
                 (metadata ((source Generated) (liveness Normal)))))
               (f_branch
                ((statements ()) (metadata ((source Generated) (liveness Dead)))))))
             (If_stm
              ((cond (Constant (Bool false)))
               (t_branch
                ((statements
                  ((Prim ((source Generated) (liveness Normal))
                    (Atomic
                     (Atomic_store
                      ((src (Constant (Int 95))) (dst (Lvalue (Variable y)))
                       (mo memory_order_seq_cst)))))))
                 (metadata ((source Generated) (liveness Dead)))))
               (f_branch
                ((statements ()) (metadata ((source Generated) (liveness Normal)))))))
             (While_loop
              ((cond (Bop Eq (Constant (Int 4)) (Constant (Int 5))))
               (body
                ((statements
                  ((Prim ((source Generated) (liveness Normal))
                    (Atomic
                     (Atomic_store
                      ((src (Constant (Int 44))) (dst (Lvalue (Variable x)))
                       (mo memory_order_seq_cst)))))))
                 (metadata ((source Generated) (liveness Dead)))))
               (kind Do_while))))))
          ((decls ())
           (stms
            ((Prim ((source Generated) (liveness Normal)) (Label loop))
             (If_stm
              ((cond (Constant (Bool true)))
               (t_branch
                ((statements ()) (metadata ((source Generated) (liveness Normal)))))
               (f_branch
                ((statements
                  ((Prim ((source Generated) (liveness Normal)) (Goto loop))))
                 (metadata ((source Generated) (liveness Dead)))))))))))))) |}]
  end )
