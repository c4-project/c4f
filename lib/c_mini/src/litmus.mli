(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Mini-model: Litmus tests

    This module declares modules and functions for manipulating litmus tests
    over act's 'mini' subset of C. *)

(** The mini-model, packaged up as a Litmus language.

    This language uses {{!Reify} Reify} for all of its pretty-printing needs. *)
module Lang :
  Act_litmus.Test_types.Basic
    with type Statement.t =
          [ `Stm of unit Statement.t
          | `Decl of Initialiser.t Act_common.C_named.t ]
     and type Program.t = unit Function.t Act_common.C_named.t
     and type Constant.t = Constant.t

(** The mini-model's full Litmus test module. *)
module Test :
  Act_litmus.Test_types.S
    with module Lang = Lang
     and type raw = Act_litmus.Test.Raw.M(Lang).t

module Pp : Act_litmus.Pp_intf.S with module Test = Test
(** Pretty-printing for the mini-model's litmus AST. *)
