(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

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

open Base
open Import

(** FIR, packaged up as a Litmus language. *)
module Lang :
  Act_litmus.Test_types.Basic
    with type Statement.t =
          [`Stm of unit Statement.t | `Decl of Initialiser.t Common.C_named.t]
     and type Program.t = unit Function.t Common.C_named.t
     and type Constant.t = Constant.t

(** FIR's full Litmus test module. *)
module Test :
  Act_litmus.Test_types.S
    with module Lang = Lang
     and type raw = Act_litmus.Test.Raw.M(Lang).t

(** To pretty-print FIR, one must convert it into a concrete language; see
    the various 'reify' modules in {!Act_litmus_c}. *)

module Var : sig
  module Record : sig
    (** Type of records returned by variable queries.

        These include the type of the variable and its intended index in any
        parameter list. *)
    type t = {ty: Type.t; param_index: int} [@@deriving compare, sexp]
  end

  val make_alist :
    Test.t -> (Common.Litmus_id.t, Record.t) List.Assoc.t Or_error.t
  (** [make_alist vast] tries to make an associative list from the global and
      local variables present in [vast] to {!Record}s. The resulting list is
      ordered such that each global appears in the same order that it appears
      in the threads' parameter lists, and each local is in increasing thread
      ID and declaration order. *)
end
