(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Aux = Act_litmus.Aux
module Post = Act_litmus.Postcondition
module Ac = Act_common

module Sbsc = struct
  let str_local (tid : int) (str : string) : Ac.Litmus_id.t =
    Ac.Litmus_id.local tid (Ac.C_id.of_string str)

  let a (tid : int) = str_local tid "a"

  let postcondition : int Post.t Lazy.t =
    lazy
      Post.(
        make ~quantifier:Exists
          ~predicate:Pred.Infix.(a 0 ==? 0 && a 1 ==? 1))

  let aux : int Aux.t Lazy.t =
    let x = Ac.C_id.of_string "x" in
    let y = Ac.C_id.of_string "y" in
    let init : (Ac.C_id.t, int) List.Assoc.t = [(x, 0); (y, 0)] in
    let locations : Ac.C_id.t list = [x; y] in
    Lazy.Let_syntax.(
      let%map postcondition = postcondition in
      Aux.make ~init ~postcondition ~locations ())

  let threads : string list Lazy.t =
    lazy ["(this was thread 0)"; "(this was thread 1)"]

  let test : (int, string) Act_litmus.Test.Raw.t Lazy.t =
    Lazy.Let_syntax.(
      let%map aux = aux and threads = threads in
      Act_litmus.Test.Raw.make ~name:"SBSC" ~aux ~threads)
end
