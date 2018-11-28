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

(** [My_monad] contains generic monad extensions. *)

open Base

(** [Extensions] contains extensions for a [Monad.S]. *)
module type Extensions = sig
  (** [t] is the type of the extended monad. *)
  type 'a t

  (** [when_m predicate ~f] returns [f ()] when [predicate] is true,
     and [return ()] otherwise. *)
  val when_m   : bool -> f:(unit -> unit t) -> unit t
  (** [unless_m predicate ~f] returns [f ()] when [predicate] is
     false, and [return ()] otherwise. *)
  val unless_m : bool -> f:(unit -> unit t) -> unit t
end

(** [Extend] creates [Extensions] for a a [Monad.S]. *)
module Extend
  : functor (M : Monad.S) -> Extensions with type 'a t := 'a M.t
;;

(** [S2_to_S] demotes an arity-2 monad [M] to an arity-1 one,
    fixing its second type to be [B.t]. *)
module S2_to_S
  : functor (M : Monad.S2)
    -> functor (B : T)
      -> Monad.S with type 'a t := ('a, B.t) M.t
;;
