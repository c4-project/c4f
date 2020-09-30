(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Common imports *)

module Accessor = Accessor_base
include Accessor.O
module Common = Act_common
module Fir = Act_fir
module Fuzz = Act_fuzz
module Litmus_c = Act_litmus_c
module Utils = Act_utils
