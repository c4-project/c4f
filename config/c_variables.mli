(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation
   files (the "Software"), to deal in the Software without
   restriction, including without limitation the rights to use, copy,
   modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

(** Interface for passing information about C variables to, and
   around, act.

    Several of act's components need auxiliary information about the
   variables of a C program or litmus test.  Most of this information
   corresponds to facts about a litmus test that get erased when it is
   converted to a compilable C file.

    For example, we may need to know which variables in a compilable C
   file correspond to shared variables in its litmus test, or the
   values the litmus test used to initialise those variables.

    This module contains various sub-modules for handling this
   auxiliary information.  *)

open Utils

(** Flag used to mark C variables with information about their scope. *)
module Scope : sig
  type t =
    | Unknown
    | Local
    | Global
  [@@deriving sexp, compare, equal]
  ;;

  val brand : t -> C_identifier.Set.t -> t C_identifier.Map.t
  (** [brand scope vars] lifts [vars] to a string-to-scope map,
      applying [scope] to each variable. *)

  val make_map_opt
    :  ?locals:C_identifier.Set.t
    -> ?globals:C_identifier.Set.t
    -> unit
    -> t C_identifier.Map.t option
    (** [make_map_opt ?locals ?globals ()] makes a variable-to-scope
        map by merging the locals set [locals] and the globals set
        [globals]. *)
end

module Map : sig
  type t = Scope.t C_identifier.Map.t (* for now *)
end
