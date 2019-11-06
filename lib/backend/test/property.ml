(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

let%expect_test "all properties have documentation" =
  let num_passes =
    Act_backend.Property.names |> Lazy.force
    |> List.map
         ~f:
           (List.Assoc.mem Act_backend.Property.tree_docs
              ~equal:String.Caseless.equal)
    |> List.count ~f:not
  in
  Fmt.pr "@[<v>%d@]@." num_passes ;
  [%expect {| 0 |}]