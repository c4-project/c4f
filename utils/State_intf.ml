(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

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

(** Haskell-style state monads and monad transformers *)

open Core

(** [fold_mapper] is the type of fold-mapping functions. *)
type ('s, 't, 'c) fold_mapper =
  f : ('s -> 't -> ('s * 't))
  -> init : 's
  -> 'c
  -> ('s * 'c)
;;

(** [S_common] is the signature common to [S] and [S_transform]. *)
module type S_common = sig
  (** Both [S] and [S_transform] are extended monads. *)
  include Monad.S
  include MyMonad.Extensions with type 'a t := 'a t

  (** [state] is the type of the inner state record. *)
  type state

  (*
   * Building stateful computations
   *)

  (** [make] creates a context-sensitive computation that can modify
      both the current context and the data passing through. *)
  val make : (state -> (state * 'a)) -> 'a t

  (** [peek] creates a context-sensitive computation that can look at
      the current context, but not modify it. *)
  val peek : (state -> 'a) -> 'a t

  (** [modify] creates a context-sensitive computation that can look at
      and modify the current context. *)
  val modify : (state -> state) -> unit t
end

(** [S] is the signature of state monads.

    State monads form a way to thread a modifiable, readable state
   record through a computation without resorting to mutability.  At
   any point in a state-monad computation, we can [peek] at the
   present value of the state, or [modify] it, or do both. *)
module type S = sig
  include S_common

  (*
   * Running a stateful computation
   *)

  (** [run] unfolds a [t] into a function from context
      to final result.  To get the final context, call [peek]
      at the end of the computation. *)
  val run : 'a t -> state -> 'a

  (** [on_fold_map] lifts a stateful computation over a fold-mapper. *)
  val on_fold_map
    : (state, 'a, 'b) fold_mapper -> ('a -> 'a t) -> 'b -> 'b t
  ;;
end

(** [S_transform] is the signature of state monad transformers.

    Unlike an [S] monad, each [S_transform] computation returns its
   value inside another monad---for example, [Or_error].  We can use
   this to build computations that are both stateful and can fail, for
   instance. *)
module type S_transform = sig
  include S_common

  (** [Inner] is the monad to which we're adding state. *)
  module Inner : Monad.S

  (** [Monadic] contains a version of the state monad interface that
      can interact with the inner monad ([Inner]) this state transformer is
      overlaying.

      Typically, this will be an error monad, like [Or_error]. *)
  module Monadic : sig
    (** [make] creates a stateful computation that produces a
       computation in the [Inner] monad.  This computation can modify
       both the current state and the data passing through. *)
    val make : (state -> (state * 'a) Inner.t) -> 'a t

    (** [peek] creates a stateful computation that produces a
       computation in the [Inner] monad.  This computation can see the
       current state, but not modify it. *)
    val peek : (state -> 'a Inner.t) -> 'a t

    (** [modify] creates a stateful computation that produces a
       computation in the [Inner] monad.  This computation can look at
       and modify the current context. *)
    val modify : (state -> state Inner.t) -> unit t

    (** [return] lifts an [Inner] computation into a stateful one.

        For example, in [Or_error], this lifts a possibly-failing
        computation to a context-sensitive one. *)
    val return : 'a Inner.t -> 'a t
  end

  (*
   * Running a stateful computation
   *)

  (** [run] unfolds a [t] into a function from context
      to final result.  To get the final context, call [peek]
      at the end of the computation. *)
  val run : 'a t -> state -> 'a Inner.t
end

(** [Basic] is the signature that must be implemented by
    state systems being lifted into [S] instances. *)
module type Basic = Base.T.T

(** [Basic_transform] is the signature that must be implemented
    by state systems being lifted into [S_transform] instances. *)
module type Basic_transform = sig
  include Basic

  (** [Inner] is the monad to which we're adding state. *)
  module Inner : Monad.S
end

(** [State] is the part of this interface file that appears in
    [State.mli]. *)
module type State = sig
  module type S = S
  module type S_transform = S_transform
  module type Basic = Basic
  module type Basic_transform = Basic_transform

  (** [Make] makes an [S] from a [Basic]. *)
  module Make : functor (B : Basic) -> S with type state = B.t

  (** [Make_transform] makes an [S_transform] from a
     [Basic_transform]. *)
  module Make_transform
    : functor (B : Basic_transform)
      -> S_transform with type state = B.t and module Inner = B.Inner
  ;;
end
