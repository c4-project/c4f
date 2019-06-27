(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

module type S = sig
  val rewrite_all :
       Act_c.Mini.Function.t Act_c.Mini_intf.id_assoc
    -> context:Context.t
    -> Act_c.Mini.Function.t Act_c.Mini_intf.id_assoc Or_error.t
  (** [rewrite_all fs ~context] rewrites all functions in [fs], using the
      mappings in [context] to resolve lifted locals. *)
end

module Vars_as_globals : S
(** Function rewriter for the 'vars as globals' flavour of delitmus. It
    lowers all global references from pointers to values. *)

module Vars_as_parameters : S
(** Function rewriter for the 'vars as parameters' flavour of delitmus. It
    raises all local references from values to pointers. *)
