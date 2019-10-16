(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

(** A functor that constructs a partial conversion function from one Litmus
    AST to another. *)
module Make (B : sig
  module From : Test_types.S
  (** The Litmus language from which we're converting. *)

  module To : Test_types.S
  (** The Litmus language to which we're converting. *)

  val constant : From.Lang.Constant.t -> To.Lang.Constant.t Or_error.t
  (** [constant k] tries to convert [k] to the new language. *)

  val program : From.Lang.Program.t -> To.Lang.Program.t Or_error.t
  (** [constant k] tries to convert [k] to the new language. *)
end) : sig
  val convert_post :
       B.From.Lang.Constant.t Postcondition.t
    -> B.To.Lang.Constant.t Postcondition.t Or_error.t
  (** [convert_post pc] tries to convert a postcondition [pc] to the new
      language. *)

  val convert : B.From.t -> B.To.t Or_error.t
  (** [convert test] tries to convert a whole Litmus test [test] to the new
      language. *)
end
