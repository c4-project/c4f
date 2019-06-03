(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to permit
   persons to whom the Software is furnished to do so, subject to the
   following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
   NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
   DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
   OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
   USE OR OTHER DEALINGS IN THE SOFTWARE. *)

(** Litmus AST: base modules and functors.

    These parts of the litmus AST have no module-level dependency on the
    underlying language of the litmus tests. *)

open Base
open Ast_base_intf

(** Directly-parametrised AST for basic predicate elements.

    The distinction between [Pred_elt] and {{!Pred} Pred} mainly exists to
    make conversion to and from other languages, like [Blang], easier. *)
module Pred_elt : sig
  type 'const t = Eq of Id.t * 'const
  [@@deriving sexp, compare, equal, quickcheck]

  include
    S_pred_elt
    with type id := Id.t
     and type 'const t := 'const t
     and type 'const elt := 'const

  (** {3 Constructors} *)

  val ( ==? ) : Id.t -> 'const -> 'const t

  (** {3 Traversals} *)

  (** Traversing monadically over all identifiers in a predicate element,
      fixing the constant type to some [Const.t]. *)
  module On_identifiers (Const : T) :
    Travesty.Traversable.S0 with type t = Const.t t and type Elt.t = Id.t

  (** Traversing monadically over all constants in a predicate element. *)
  module On_constants :
    Travesty.Traversable.S1 with type 'const t := 'const t
end

(** Directly-parametrised AST for predicates. *)
module Pred : sig
  (** Type of Litmus predicates. *)
  type 'const t =
    | Bracket of 'const t
    | Or of 'const t * 'const t
    | And of 'const t * 'const t
    | Elt of 'const Pred_elt.t
  [@@deriving sexp, compare, equal, quickcheck]

  include
    S_pred
    with type 'const t := 'const t
     and type 'const elt := 'const Pred_elt.t

  (** {3 Constructors} *)

  val ( && ) : 'const t -> 'const t -> 'const t

  val ( || ) : 'const t -> 'const t -> 'const t

  (** {3 Traversals} *)

  (** Traversing monadically over all identifiers in a predicate,
      fixing the constant type to some [Const.t]. *)
  module On_identifiers (Const : T) :
    Travesty.Traversable.S0 with type t = Const.t t and type Elt.t = Id.t

  (** Traversing monadically over all constants in a predicate. *)
  module On_constants :
    Travesty.Traversable.S1 with type 'const t := 'const t
end

(** Directly-parametrised AST for postconditions. *)
module Postcondition : sig
  (** Type of Litmus postconditions. *)
  type 'const t = {quantifier: [`Exists]; predicate: 'const Pred.t}
  [@@deriving sexp, compare, equal, quickcheck]

  include
    S_postcondition
    with type 'const t := 'const t
     and type 'const pred := 'const Pred.t

  (** {3 Traversals} *)

  (** Traversing monadically over all identifiers in a postcondition,
      fixing the constant type to some [Const.t]. *)
  module On_identifiers (Const : T) :
    Travesty.Traversable.S0 with type t = Const.t t and type Elt.t = Id.t

  (** Traversing monadically over all constants in a postcondition. *)
  module On_constants :
    Travesty.Traversable.S1 with type 'const t := 'const t
end
