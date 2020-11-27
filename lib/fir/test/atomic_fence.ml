(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open struct
  module Src = Act_fir
end

let%expect_test "On_mem_orders traverses properly" =
  Src.Atomic_fence.(
    let subject = make ~mode:Thread ~mo:Seq_cst in
    On_mem_orders.iter subject ~f:(fun x ->
        Stdio.print_s (Src.Mem_order.sexp_of_t x))) ;
  [%expect {| memory_order_seq_cst |}]
