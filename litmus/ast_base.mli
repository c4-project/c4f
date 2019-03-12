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

(** Litmus AST: base modules and functors.

    These parts of the litmus AST have no module-level dependency on the
    underlying language of the litmus tests. *)

open Core_kernel
open Utils

include module type of Ast_base_intf

module Id : sig
  type t =
    | Local of int * C_identifier.t
    | Global of C_identifier.t
  [@@deriving compare, sexp, quickcheck]
  ;;

  val try_parse : string -> t Or_error.t
  (** [try_parse str] tries to parse [str] as a Litmus identifier. *)

  include Stringable.S with type t := t
  (** Litmus identifiers can be converted to and from strings.
      Note that conversion from strings can fail if the C identifier
      parts don't obey C identifier validation. *)

  include Comparable.S with type t := t
  (** Litmus identifiers suit various comparable scenarios, such as
     map keys. *)

  val to_memalloy_id : t -> C_identifier.t
  (** [to_memalloy_id id] converts [id] to the corresponding
      memalloy executable-C global variable name.

      This is [x] where [id = Global x], and ["tXY"] where
      [id = Local (X, Y)]. *)
end

(** Directly-parametrised AST for basic predicate elements.

    The distinction between [Pred_elt] and {{!Pred}Pred} mainly
    exists to make conversion to and from other languages, like
    [Blang], easier. *)
module Pred_elt : sig
  type 'const t =
    | Eq of Id.t * 'const
  [@@deriving sexp, compare, equal, quickcheck]
  ;;

  include S_pred_elt with type id := Id.t
                      and type 'const t := 'const t
                      and type 'const elt := 'const
  ;;

  module On_constants : Travesty.Traversable.S1_container
    with type 'const t := 'const t
    (** Traversing monadically over all constants in a predicate
        element. *)
end

(** Directly-parametrised AST for predicates. *)
module Pred : sig
  type 'const t =
    | Bracket of 'const t
    | Or of 'const t * 'const t
    | And of 'const t * 'const t
    | Elt of 'const Pred_elt.t
  [@@deriving sexp, compare, equal, quickcheck]
  (** Type of Litmus predicates. *)

  include S_pred with type 'const t := 'const t
                  and type 'const elt := 'const Pred_elt.t

  module On_constants : Travesty.Traversable.S1_container
    with type 'const t := 'const t
    (** Traversing monadically over all constants in a predicate. *)
end

(** Directly-parametrised AST for postconditions. *)
module Postcondition : sig
  type 'const t =
    { quantifier : [ `Exists ]
    ; predicate  : 'const Pred.t
    }
  [@@deriving sexp, compare, equal, quickcheck]
  (** Type of Litmus postconditions. *)

  include S_postcondition
    with type 'const t := 'const t and type 'const pred := 'const Pred.t

  module On_constants : Travesty.Traversable.S1_container
    with type 'const t := 'const t
    (** Traversing monadically over all constants in a postcondition. *)
end
