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

(** Haskell-style state monads and monad transformers

    State monads form a way to thread a modifiable, readable state
   record through a computation without resorting to mutability.  At
   any point in a state-monad computation, we can [peek] at the
   present value of the state, or [modify] it, or do both.

    State monad transformers attach onto an existing monad, and allow
   the stateful computations to return and handle values inside that
   monad.  For example, transforming [On_error] provides stateful,
   potentially-failing computations.

    We provide four signatures for state monads: two sets of state
   monad and monad transformer; one corresponding to the situation
   where the state type is fixed at the module level ([S],
   [S_transform]), and one leaving the state type as part of the monad
   type ([S2], [S2_transform].  Each has a corresponding make
   functor---except [S2], which has a direct implementation [M2].

    We also provide two functors, [To_S_transform] and [To_S], for
   fixing the state type in an arity-2 monad after the fact.

    To reduce a state transformer to a state monad, apply it to
   [Monad.Ident]---this is how we implement [Make] and [M2].  *)

open Core

(** [Generic_types] contains generic versions of the types used in
    [Generic_builders] and [Generic_runners]. *)
module type Generic_types = sig
  (** [t] is the type of the state monad. *)
  type ('a, 's) t
  (** [final] is the type of returned results.  In transformers, this
     becomes ['a Inner.t]; otherwise, it becomes just ['a]. *)
  type 'a final
  (** [state] is the type used to represent the state outside of its
     monad.  In [S], ['s state] becomes [x] for some type [x]; in
     [S2], ['s state] becomes ['s]. *)
  type 's state
end

(** [Generic_builders] contains generic versions of the 'builder'
    functions common to all state monad signatures. *)
module type Generic_builders = sig
  include Generic_types

  (** [make] creates a context-sensitive computation that can modify
      both the current context and the data passing through. *)
  val make : ('s state -> ('s state * 'a) final) -> ('a, 's) t

  (** [peek] creates a context-sensitive computation that can look at
      the current context, but not modify it. *)
  val peek : ('s state -> 'a final) -> ('a, 's) t

  (** [modify] creates a context-sensitive computation that can look at
      and modify the current context. *)
  val modify : ('s state -> 's state final) -> (unit, 's) t

  (** [return] lifts a value or monad into a stateful computation. *)
  val return : 'a final -> ('a, 's) t
end

(** [Generic_runners] contains generic versions of the 'runner'
    functions common to all state monad signatures. *)
module type Generic_runners = sig
  include Generic_types

  (** [run'] unfolds a [t] into a function from context
      to final state and result. *)
  val run' : ('a, 's) t -> 's state -> ('s state * 'a) final

  (** [run] unfolds a [t] into a function from context to final
     result.  To get the final context, use [run'] or call [peek] at
     the end of the computation. *)
  val run : ('a, 's) t -> 's state -> 'a final
end

(** [Fix] contains the signature for fixpoint builders. *)
module type Fix = sig
  type ('a, 's) t

  (** [fix ~f init] builds a fixed point on [f].

      At each step, [f] is passed a continuation [mu] and a value [a].
      It may choose to return a recursive application of [mu], or
      some value derived from [a].

      To begin with, [f] is applied to [mu] and [init]. *)
  val fix
    :  f:(('a -> ('a, 's) t) -> 'a -> ('a, 's) t)
    -> 'a
    -> ('a, 's) t
  ;;
end

(** [Generic] contains the signature bits common to all state monad
    signatures. *)
module type Generic = sig
  include Generic_builders
  include Generic_runners with type ('a, 's) t := ('a, 's) t
                           and type 'a final := 'a final
                           and type 's state := 's state
  ;;
  include Fix with type ('a, 's) t := ('a, 's) t
end


(** [Generic_transform] contains the signature bits common to all
   state transformers. *)
module type Generic_transform = sig
  include Generic_builders with type 'a final := 'a

  (** [Inner] is the monad to which we're adding state. *)
  module Inner : Monad.S

  (** State transformers have the same runner signatures as
      state monads, but lifted into the inner monad. *)
  include Generic_runners with type ('a, 's) t := ('a, 's) t
                           and type 'a final := 'a Inner.t
                           and type 's state := 's state
  ;;

  include Fix with type ('a, 's) t := ('a, 's) t

  (** [Monadic] contains a version of the state monad interface that
     can interact with the inner monad ([Inner]) this state
     transformer is overlaying. *)
  module Monadic : Generic_builders with type 'a state := 'a state
                                     and type 'a final := 'a Inner.t
                                     and type ('a, 's) t := ('a, 's) t
end

(** [S] is the signature of state monads parametrised over their
    value, but with a fixed state type. *)
module type S = sig
  (** [state] is the fixed state type. *)
  type state

  include Monad.S
  include My_monad.Extensions with type 'a t := 'a t
  include Generic with type ('a, 's) t := 'a t
                   and type 's state := state
                   and type 'a final := 'a
end

(** [S2] is the signature of state monads parametrised over both
    value and state types. *)
module type S2 = sig
  include Monad.S2
  include Generic with type ('a, 's) t := ('a, 's) t
                   and type 's state := 's
                   and type 'a final := 'a
end

(** [S_transform] is the signature of state monad transformers with a
    fixed state type.

    Unlike an [S] monad, each [S_transform] computation returns its
   value inside another monad---for example, [Or_error].  We can use
   this to build computations that are both stateful and can fail, for
   instance. *)
module type S_transform = sig
  (** [state] is the fixed state type. *)
  type state

  include Monad.S
  include My_monad.Extensions with type 'a t := 'a t
  include Generic_transform with type ('a, 's) t := 'a t
                             and type 's state := state
end

(** [Basic_transform] is the signature that must be implemented
    by state systems being lifted into [S_transform] instances. *)
module type Basic_transform = sig
  (** [t] is the type of the state. *)
  type t

  (** [Inner] is the monad to which we're adding state. *)
  module Inner : Monad.S
end

(** [S2_transform] is the signature of state transformers parametrised
   over both value and state types. *)
module type S2_transform = sig
  include Monad.S2
  include Generic_transform with type ('a, 's) t := ('a, 's) t
                             and type 's state := 's
end

(** [State] is the part of this interface file that appears in
    [State.mli]. *)
module type State = sig
  module type S = S
  module type S_transform = S_transform
  module type S2 = S2
  module type S2_transform = S2_transform
  module type Basic_transform = Basic_transform

  (** [M2] is a basic implementation of [S2]. *)
  module M2 : S2

  (** [To_S_transform] flattens a [S2_transform] into an [S_transform]
     by fixing the state type to [B.t]. *)
  module To_S_transform
    : functor (M : S2_transform)
      -> functor (B : Base.T) -> S_transform with type state = B.t
  ;;

  (** [To_S] flattens a [S2] into an [S] by fixing the state type
      to [B.t]. *)
  module To_S
    : functor (M : S2)
      -> functor (B : Base.T) -> S with type state = B.t
  ;;

  (** [Make2_transform] makes an [S2_transform] from a [Monad.S]. *)
  module Make2_transform
    : functor (M : Monad.S)
      -> S2_transform with module Inner = M
  ;;

  (** [Make] makes an [S] from a single state type. *)
  module Make : functor (B : Base.T.T) -> S with type state = B.t

  (** [Make_transform] makes an [S_transform] from a
     [Basic_transform]. *)
  module Make_transform
    : functor (B : Basic_transform)
      -> S_transform with type state = B.t and module Inner = B.Inner
  ;;
end
