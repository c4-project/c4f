(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Header = C4f_litmus.Header
module Post = C4f_litmus.Postcondition
module Pred = C4f_litmus.Predicate
module Ac = C4f_common

module Sbsc = struct
  let str_local (tid : int) (str : string) : Ac.Litmus_id.t =
    Ac.Litmus_id.local tid (Ac.C_id.of_string str)

  let a (tid : int) = str_local tid "a"

  let postcondition : int Post.t Lazy.t =
    lazy
      Post.(
        make ~quantifier:Exists
          ~predicate:Pred.Infix.(a 0 ==? 0 && a 1 ==? 1))

  let header : int Header.t Lazy.t =
    let x = Ac.C_id.of_string "x" in
    let y = Ac.C_id.of_string "y" in
    let init : (Ac.C_id.t, int) List.Assoc.t = [(x, 0); (y, 0)] in
    let locations : Ac.C_id.t list = [x; y] in
    Lazy.Let_syntax.(
      let%map postcondition = postcondition in
      Header.make ~name:"SBSC" ~init ~postcondition ~locations ())

  let threads : string list Lazy.t =
    lazy ["(this was thread 0)"; "(this was thread 1)"]

  let test : (int, string) C4f_litmus.Test.Raw.t Lazy.t =
    Lazy.Let_syntax.(
      let%map header = header and threads = threads in
      C4f_litmus.Test.Raw.make ~header ~threads)
end
