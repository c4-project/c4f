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

(** Haskell-style state monads

    State monads form a way to thread a modifiable, readable state
   record through a computation without resorting to mutability.  At
   any point in a state-monad computation, we can [peek] at the
   present value of the state, or [modify] it, or do both.

    We provide two signatures for state monads: one corresponding to
   the situation where the state type is fixed at the module level
   ([S]), and one leaving the state type as part of the monad type
   ([S2]).  [S] has a corresponding make functor; [S2] has a direct
   implementation ([M2]).

    We also provide a functor [To_S] for fixing the state type in an
    arity-2 monad after the fact. *)

open Core_kernel

(** [Generic] contains the signature bits common to all state monad
    signatures. *)
module type Generic = sig
  include State_transform_intf.Generic_builders
  include State_transform_intf.Generic_runners
    with type ('a, 's) t := ('a, 's) t
     and type 'a final := 'a final
     and type 's state := 's state
  ;;
  include State_transform_intf.Fix with type ('a, 's) t := ('a, 's) t
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

(** [State] is the part of this interface file that appears in
    [State.mli]. *)
module type State = sig
  module type S = S
  module type S2 = S2

  (** [M2] is a basic implementation of [S2]. *)
  module M2 : S2

  (** [To_S] flattens a [S2] into an [S] by fixing the state type
      to [B.t]. *)
  module To_S
    : functor (M : S2)
      -> functor (B : Base.T) -> S with type state = B.t
                                    and type 'a t = ('a, B.t) M.t
  ;;

  (** [Make] makes an [S] from a single state type. *)
  module Make : functor (B : Base.T.T) -> S with type state = B.t
end
