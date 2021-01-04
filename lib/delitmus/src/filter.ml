(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

let prelude : string list =
  [ "// <!> Auto-generated from a litmus test by ACT."
  ; "#include <stdatomic.h>"
  ; "#include <stdbool.h>"
  ; "" ]

let pp_prelude (type a) : a Fmt.t = Fmt.(const (list ~sep:sp string) prelude)

let pp_unit : Litmus_c.Ast.Translation_unit.t Fmt.t =
  Fmt.(vbox (pp_prelude ++ Litmus_c.Ast.Translation_unit.pp))

let c_of_output : Output.t -> Litmus_c.Ast.Translation_unit.t =
  Fn.compose Litmus_c.Reify.program Output.program

let pp_del : Output.t Fmt.t = Fmt.(using c_of_output pp_unit)

let run (input : Plumbing.Input.t) (output : Plumbing.Output.t)
    ~(config : Config.t) : Output.t Or_error.t =
  let (module R) = Config.to_runner config in
  Or_error.Let_syntax.(
    let%bind test = Litmus_c.Frontend.Fir.load input in
    let%bind dl = R.run test in
    let%map () = Utils.My_format.odump output (Fmt.vbox pp_del) dl in
    dl)
