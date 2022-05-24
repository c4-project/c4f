(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

module type S = sig
  val rewrite_all :
       unit Fir.Function.t Common.C_named.t list
    -> context:Context.t
    -> unit Fir.Function.t Common.C_named.t list Or_error.t
  (** [rewrite_all fs ~context] rewrites all functions in [fs], using the
      mappings in [context] to resolve lifted locals. *)
end

(** Function rewriter for the 'vars as globals' flavour of delitmus. It
    lowers all global references from pointers to values. *)
module Vars_as_globals : S

(** Function rewriter for the 'vars as parameters' flavour of delitmus. It
    raises all local references from values to pointers. *)
module Vars_as_parameters : S
