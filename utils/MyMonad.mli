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

(** Extensions to Core monads *)

open Core

(** [Extensions] is the interface of monad extensions. *)
module type Extensions = sig
  type 'a t

  (** [mapM] binds a function [f] across a list, from left to right,
      collating the monadic context over the whole result list. *)
  val mapM
    :  f:('a -> 'b t)
    -> ('a list) t
    -> ('b list) t

  (** [mapiM] behaves as [bindL], but also supplies the index of each
      item to [f]. *)
  val mapiM
    :  f:(int -> 'a -> 'b t)
    -> ('a list) t
    -> ('b list) t

  (** [tapM ~f x] executes [f] for its monadic effect, then returns
      [x]. *)
  val tapM
    :  f:('a -> unit t)
    -> 'a
    -> 'a t

  (** [tap ~f x] executes [f] for its side-effect, then returns
      [x]. *)
  val tap
    :  f:('a -> unit)
    -> 'a
    -> 'a t
end

(** [Extend] extends monad [M] with the extensions in [Extensions]. *)
module Extend
  : functor (M : Monad.S) -> Extensions with type 'a t := 'a M.t

module MyOption : Extensions with type 'a t = 'a option
module MyOr_error : Extensions with type 'a t = 'a Or_error.t

(* For [MyList], see [MyContainers]. *)

