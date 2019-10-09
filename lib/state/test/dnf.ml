(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Src = Act_state
module Tx = Travesty_base_exts

module Examples = struct
  let entry = [("foo", "1"); ("0:bar", "2"); ("1:baz", "3")]

  let entries =
    [ entry
    ; [("foo", "1"); ("0:bar", "4"); ("1:baz", "9")]
    ; [("foo", "1"); ("0:bar", "10")] ]

  let obs =
    Src.Observation.empty
    |> Observation.Test_utils.add_entries_exn entries
    |> Observation.Test_utils.add_entries_exn ~tag:Counter_example
         [[("foo", "6"); ("0:bar", "5"); ("1:baz", "9")]]
    |> Observation.Test_utils.add_entries_exn ~tag:Witness
         [[("foo", "6"); ("1:baz", "53")]]
end

let print_predicate : string Act_litmus.Predicate.t -> unit =
  Fmt.pr "@[%a@]@." (Act_litmus.Predicate.pp ~pp_const:String.pp)

let print_postcondition : string Act_litmus.Postcondition.t -> unit =
  Fmt.pr "@[%a@]@." (Act_litmus.Postcondition.pp ~pp_const:String.pp)

let%test_module "predicate_of_state" =
  ( module struct
    let test (input : (string, string) List.Assoc.t) : unit =
      let entry = Entry.Test_utils.entry_exn input in
      let output = Src.Dnf.predicate_of_state entry in
      print_predicate output

    let%expect_test "sample entry" =
      test Examples.entry ;
      [%expect {| foo == 1 /\ (0:bar == 2 /\ 1:baz == 3) |}]
  end )

let%test_module "predicate_of_states" =
  ( module struct
    let test (input : (string, string) List.Assoc.t list) : unit =
      let entries = Entry.Test_utils.entries_exn input in
      let output = Src.Dnf.predicate_of_states entries in
      print_predicate output

    let%expect_test "empty set" = test [] ; [%expect {| false |}]

    let%expect_test "set of empty observation" =
      test [[]] ;
      [%expect {| true |}]

    let%expect_test "sample set" =
      test Examples.entries ;
      [%expect
        {|
        (foo == 1 /\ 0:bar == 10) \/
        ((foo == 1 /\ (0:bar == 2 /\ 1:baz == 3)) \/
         (foo == 1 /\ (0:bar == 4 /\ 1:baz == 9))) |}]
  end )

let%test_module "convert_states" =
  ( module struct
    let test (input : (string, string) List.Assoc.t list) : unit =
      let entries = Entry.Test_utils.entries_exn input in
      let output = Src.Dnf.convert_states entries in
      print_postcondition output

    let%expect_test "empty set" = test [] ; [%expect {| forall (false) |}]

    let%expect_test "set of empty entry" =
      test [[]] ;
      [%expect {| forall (true) |}]

    let%expect_test "sample set" =
      let raw_entries =
        [ [("foo", "1"); ("0:bar", "2"); ("1:baz", "3")]
        ; [("foo", "1"); ("0:bar", "4"); ("1:baz", "9")]
        ; [("foo", "1"); ("0:bar", "10")] ]
      in
      test raw_entries ;
      [%expect
        {|
        forall
        ((foo == 1 /\ 0:bar == 10) \/
         ((foo == 1 /\ (0:bar == 2 /\ 1:baz == 3)) \/
          (foo == 1 /\ (0:bar == 4 /\ 1:baz == 9)))) |}]
  end )

let%test_module "convert" =
  ( module struct
    let test (input : Src.Observation.t) : unit =
      let output = Src.Dnf.convert input in
      print_postcondition output

    let%expect_test "empty observation" =
      test Src.Observation.empty ;
      [%expect {| forall (false) |}]

    let%expect_test "example observation" =
      test Examples.obs ; [%expect {|
        forall
        ((foo == 1 /\ 0:bar == 10) \/
         ((foo == 1 /\ (0:bar == 2 /\ 1:baz == 3)) \/
          (foo == 1 /\ (0:bar == 4 /\ 1:baz == 9)) \/
          (foo == 6 /\ (0:bar == 5 /\ 1:baz == 9)) \/ (foo == 6 /\ 1:baz == 53))) |}]
  end )
