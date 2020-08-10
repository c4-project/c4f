(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** FIR: Litmus tests

    This module declares modules and functions for manipulating litmus tests
    over FIR directly. This is mostly used inside the fuzzer and other
    program transformers; the actual litmus language exposed outside of these
    programs is in the {!Act_litmus_c} library. *)

(** FIR, packaged up as a Litmus language. *)
module Lang :
  Act_litmus.Test_types.Basic
    with type Statement.t =
          [ `Stm of unit Statement.t
          | `Decl of Initialiser.t Act_common.C_named.t ]
     and type Program.t = unit Function.t Act_common.C_named.t
     and type Constant.t = Constant.t

(** FIR's full Litmus test module. *)
module Test :
  Act_litmus.Test_types.S
    with module Lang = Lang
     and type raw = Act_litmus.Test.Raw.M(Lang).t

(** To pretty-print FIR, one must convert it into a concrete language; see
    the various 'reify' modules in {!Act_litmus_c}. *)
