(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

module Make (B : sig
  module Constant : Act_language.Constant_types.S

  module Symbol : Act_language.Symbol_types.S
end) : sig
  val of_delitmus_aux :
       Act_delitmus.Aux.t
    -> redirect_map:B.Symbol.R_map.t
    -> B.Constant.t Act_litmus.Header.t Or_error.t
end
