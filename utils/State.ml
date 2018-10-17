(* This file is part of 'act'.

Copyright (c) 2018 by Matt Windsor

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. *)

(** Haskell-style state monads *)

open Core

module type Intf = sig
  type state

  include Monad.S
  include MyMonad.Extensions with type 'a t := 'a t

  (*
   * Building stateful computations
   *)

  (** [make] creates a context-sensitive computation that can modify
     the current context. *)
  val make : (state -> (state * 'a)) -> 'a t

  (** [peek] creates a context-sensitive computation that can look at
     the current context, but not modify it. *)
  val peek : (state -> 'a) -> 'a t

  (** [modify] creates a context-sensitive computation that can look at
     and modify the current context, but not modify the value passing
     through. *)
  val modify : (state -> state) -> 'a -> 'a t

  (*
   * Running a stateful computation
   *)

  (** [run] unfolds a [t] into a function from context
      to context and final result. *)
  val run : 'a t -> state -> (state * 'a)

  (** [run'] behaves like [run], but discards the final context. *)
  val run' : 'a t -> state -> 'a
end

module type S = sig
  type state
end

module Make (M : S)
  : Intf with type state = M.state = struct
  type state = M.state

  module Inner = struct
    type 'a t = (state -> (state * 'a))

    include Monad.Make (struct
      type nonrec 'a t = 'a t

      let map' wc ~f =
        fun state ->
          let (state', a) = wc state in
          (state', f a)
      let map = `Custom map'
      let bind wc ~f =
        fun state ->
          let (state', a) = wc state in
          (f a) state'
      let return a = fun state -> (state, a)
    end)
  end

  include Inner
  include MyMonad.Extend(Inner)

  let run = Fn.id
  let run' f ctx = f ctx |> snd
  let make = Fn.id
  let peek f ctx = ctx, f ctx
  let modify f a ctx = f ctx, a
end
