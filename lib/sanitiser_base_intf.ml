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

(** [Basic] is the standard input signature for functors creating sanitiser
    passes. *)
module type Basic = sig
  (** [Lang] provides language-specific functionality. *)
  module Lang : Language.Definition.S

  (** [Ctx] is the sanitiser's state monad, specialised over [Lang]. *)
  module Ctx : Sanitiser_ctx.S with module Lang := Lang

  (** [Program_container] is the container used to hold the one or more
      programs being sanitised. *)
  module Program_container : Travesty.Traversable.S1
end

(** [S_location] is the standard signature for location passes. *)
module type S_location = sig
  include Basic

  val on_location : Lang.Location.t -> Lang.Location.t Ctx.t
  (** [on_location loc] runs this sanitiser pass over [loc]. *)
end

(** [S_instruction] is the standard signature for instruction passes. *)
module type S_instruction = sig
  include Basic

  val on_instruction : Lang.Instruction.t -> Lang.Instruction.t Ctx.t
  (** [on_instruction ins] runs this sanitiser pass over [ins]. *)
end

(** [S_statement] is the standard signature for statement passes. *)
module type S_statement = sig
  include Basic

  val on_statement : Lang.Statement.t -> Lang.Statement.t Ctx.t
  (** [on_statement stm] runs this sanitiser pass over [stm]. *)
end

(** [S_program] is the standard signature for program passes. *)
module type S_program = sig
  include Basic

  val on_program : Lang.Program.t -> Lang.Program.t Ctx.t
  (** [on_program prog] runs this sanitiser pass over [prog]. *)
end

(** [S_all] is the standard signature for passes over a program container. *)
module type S_all = sig
  include Basic

  val on_all :
       Lang.Program.t Program_container.t
    -> Lang.Program.t Program_container.t Ctx.t
  (** [on_all progs] runs this sanitiser pass over [progs]. *)
end
