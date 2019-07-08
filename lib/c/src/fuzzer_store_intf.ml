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

open Act_common

type rst =
  { store: Act_c_mini.Atomic_store.t
  ; path: Act_c_mini.Path.stm_hole Act_c_mini.Path.program_path }

(** Basic template for fuzzer store action modules. *)
module type Basic = sig
  val name : Id.t
  (** The name of this store action. *)

  val default_weight : int
  (** The default weight of this store action. *)

  val forbid_already_written : bool
  (** If true, only allow stores to variables that are known not to already
      have writes. This can help avoid combinatorial explosions. *)

  module Quickcheck (Src : Act_c_mini.Env_types.S) (Dst : Act_c_mini.Env_types.S) :
    Act_utils.My_quickcheck.S_with_sexp with type t := Act_c_mini.Atomic_store.t
  (** The generator this store action uses to create stores. *)
end
