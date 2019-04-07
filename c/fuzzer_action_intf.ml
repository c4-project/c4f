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

open Core_kernel

module type Generic = sig
  type subject

  val name : Config.Id.t
  (** The name of the action, as a config identifier. *)

  val default_weight : int
  (** The default weight of the action. *)

  (** Random state on which this action depends. *)
  module Random_state : sig
    (** The type of any random state on which this action depends. *)
    type t

    val gen : subject -> t Quickcheck.Generator.t Fuzzer_state.Monad.t
    (** [gen subject] is a stateful computation that, given subject
        [subject] and the current state, generates a random amount of random
        state for this fuzzer action. *)
  end

  val available : subject -> bool Fuzzer_state.Monad.t
  (** [available subject] is a stateful computation that, given subject
      [subject] and the current state, decides whether this action can run
      (given any member of [Random_state.t]). *)

  val run : subject -> Random_state.t -> subject Fuzzer_state.Monad.t
  (** [run subject random] is a stateful computation that runs this action
      on [subject] with random state [random]. *)
end

module type S = Generic with type subject := Fuzzer_subject.Test.t
