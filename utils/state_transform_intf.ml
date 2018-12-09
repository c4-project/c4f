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

(** Haskell-style state monad transformers

    State monad transformers generalise state monads.  They attach
   onto an existing monad, and allow the stateful computations to
   return and handle values inside that monad.  For example,
   transforming [On_error] provides stateful, potentially-failing
   computations.

    We provide two signatures for state transformers: one
   corresponding to the situation where the state type is fixed at the
   module level ([S]), and one leaving the state type as part of the
   monad type (S2).  Each has a corresponding make.

    We also provide a functor, [To_S], for fixing the state type in an
   arity-2 monad after the fact.  *)

open Core_kernel

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

(** [Generic] contains the signature bits common to all
   state transformers. *)
module type Generic = sig
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

(** [S] is the signature of state monad transformers with a fixed
   state type.

    Each [S] computation returns its value inside another monad---for
   example, [Or_error].  We can use this to build computations that
   are both stateful and can fail, for instance. *)
module type S = sig
  (** [state] is the fixed state type. *)
  type state

  include Monad.S
  include My_monad.Extensions with type 'a t := 'a t
  include Generic with type ('a, 's) t := 'a t
                   and type 's state := state
  include Monad_transform.S_fixed with type 'a t := 'a t
                                   and module Inner := Inner
end

(** [Basic] is the signature that must be implemented
    by state systems being lifted into [S_transform] instances. *)
module type Basic = sig
  (** [t] is the type of the state. *)
  type t

  (** [Inner] is the monad to which we're adding state. *)
  module Inner : Monad.S
end

(** [S2] is the signature of state transformers parametrised over both
   value and state types. *)
module type S2 = sig
  include Monad.S2
  include Generic with type ('a, 's) t := ('a, 's) t
                   and type 's state := 's
end

(** [State_transform] is the part of this interface file that appears
   in [State_transform.mli]. *)
module type State_transform = sig
  module type S = S
  module type S2 = S2
  module type Basic = Basic

  (** [To_S] flattens a [S2] into an [S]
     by fixing the state type to [B.t]. *)
  module To_S
    : functor (M : S2)
      -> functor (B : Base.T)
        -> S with type state = B.t
              and type 'a t = ('a, B.t) M.t
              and module Inner = M.Inner
  ;;

  (** [Make2] makes an [S2] from a [Monad.S]. *)
  module Make2 : functor (M : Monad.S) -> S2 with module Inner = M

  (** [Make] makes an [S] from a [Basic]. *)
  module Make
    : functor (B : Basic)
      -> S with type state = B.t and module Inner = B.Inner
  ;;
end
