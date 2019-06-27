(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

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

(** [Spec] contains general interfaces for dealing with specifications of
    machines and compilers. *)

open Base

(** Specification sets, parametrised directly on the spec type.

    As 'proper' specification types have several operations needed for full
    use of specification sets, this module has very few available
    operations. See the [Set] module constructed on specification types for
    more useful functionality. *)
module Set : sig
  type 'spec t
  (** Opaque type of specification sets. *)

  val empty : 'spec t
  (** [empty] is the empty specification set. *)

  val restrict : 'spec t -> identifiers:Id.Set.t -> 'spec t

  val of_map : 'spec Map.M(Id).t -> 'spec t

  module On_specs : Travesty.Traversable_types.S1 with type 'a t = 'a t
  (** We can monadically traverse the specifications in a set. *)
end

module type S = sig
  type t

  include Spec_types.S with type Set.t = t Set.t and type t := t
end

module With_id (C : Spec_types.Common) :
  Spec_types.S_with_id with type elt = C.t
(** [With_id] is a basic implementation of [S_with_id] for specs with type
    [B.t].

    Usually, spec modules should extend [With_id] to implement the various
    accessors they expose on the spec type itself, for convenience. *)

(** [Make] makes an [S] from a [Basic]. *)
module Make (B : Spec_types.Basic) :
  S with type t = B.t and module With_id = B.With_id
