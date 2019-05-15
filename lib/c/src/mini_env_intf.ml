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

(** Mini-model: module signatures for variable typing environments *)

open Core_kernel
open Act_utils

(** Basic signature of modules carrying a variable typing environment. *)
module type Basic = sig
  val env : Mini_type.t C_identifier.Map.t
  (** [env] is a variable typing environment. *)
end

(** Extended signature of environment modules. *)
module type S = sig
  include Basic

  module Random_var : sig
    type t = C_identifier.t [@@deriving sexp_of]

    include Quickcheck.S with type t := t
  end

  (** [Random_var] allows generation of random variables from the variable
      environment. *)

  val has_atomic_int_variables : unit -> bool
  (** [has_atomic_int_variables ()] is true provided that the environment
      has at least one variable whose type is atomic-int, or a pointer
      thereto. *)

  val has_int_variables : unit -> bool
  (** [has_int_variables ()] is true provided that the environment has at
      least one variable whose type is (non-atomic) int, or a pointer
      thereto. *)

  val atomic_int_variables : unit -> Mini_type.t C_identifier.Map.t
  (** [atomic_int_variables ()] filters the environment, returning a map
      binding only variables whose type is atomic-int, or a pointer thereto. *)

  val int_variables : unit -> Mini_type.t C_identifier.Map.t
  (** [atomic_int_variables ()] filters the environment, returning a map
      binding only variables whose type is (non-atomic) int, or a pointer
      thereto. *)
end
