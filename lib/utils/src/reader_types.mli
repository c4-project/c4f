(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Type signatures of reader monads. *)

open Base

(** Generic signature of reader monad transformers. *)
module type Generic = sig
  (** Type of the reader monad. *)
  type ('r, 'k) t

  (** Type of contexts. *)
  type 'k ctx

  module Inner : sig
    type ('r, 'k) m := ('r, 'k) t

    (** Type of the transformed monad. *)
    type 'r t

    val lift : ('k ctx -> 'r t) -> ('r, 'k) m
    (** [lift m] lifts a monadic function [f] to a reader monad. *)

    val return : 'r t -> ('r, 'k) m
    (** [return m] lifts a monadic computation [f] to a reader monad. *)
  end

  val lift : ('k ctx -> 'r) -> ('r, 'k) t
  (** [lift f] lifts a raw, pure function [f] to a reader monad. *)

  val run : ('r, 'k) t -> ctx:'k ctx -> 'r Inner.t
  (** [run m ~ctx] runs the reader monad [m] over [ctx]. *)

  (** Reader monads are monads. *)
  include Monad.S2 with type ('r, 'k) t := ('r, 'k) t

  val ( let+ ) : ('a, 'k) t -> ('a -> 'b) -> ('b, 'k) t
  (** Native OCaml map syntax. *)

  val ( let* ) : ('a, 'k) t -> ('a -> ('b, 'k) t) -> ('b, 'k) t
  (** Native OCaml bind syntax. *)
end

(** Signature of arity-2 reader monad transformers. *)
module type S2 = Generic with type 'k ctx := 'k

(** Signature of arity-1 reader monad transformers. *)
module type S = sig
  (** Type of the reader monad. *)
  type 'r t

  (** Type of contexts. *)
  type ctx

  include Generic with type ('r, 'k) t := 'r t and type 'k ctx := ctx
end
