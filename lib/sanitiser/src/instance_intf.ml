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

(** Assembly sanitisation *)

open Base

(** [Basic] describes the base functionality we need to supply to a
    sanitiser.

    It includes both a [Hook] that inserts language-specific sanitisers into
    the sanitiser, and a description of how we split the incoming assembly
    into programs and traverse over them.

    The dependency on a separate [Program_container] lets us adapt the
    sanitiser to single-program and multi-program contexts more easily. *)
module type Basic = sig
  module Hook : Hook.S

  val split :
       Hook.Lang.Program.t
    -> Hook.Lang.Program.t Hook.Program_container.t Or_error.t
  (** [split] splits an assembly script up into the one or more programs
      we'll be sanitising. *)
end

(** [S] is the interface to a fully-built sanitiser. *)
module type S = sig
  (** [Lang] is the language over which we are sanitising. *)
  module Lang : Act_language.Definition.S

  module Warn :
    Warn.S with type t = Lang.Element.t Warn.t and type elt = Lang.Element.t

  (** [Program_container] describes the container that the sanitised program
      or programs are held in. *)
  module Program_container : Container.S1

  module Output :
    Output.S
    with type listing := Lang.Program.t
     and type ('w, 'l) program := ('w, 'l) Output.Program.t
     and type warn_elt := Lang.Element.t
     and type 'l pc := 'l Program_container.t
     and type rmap := Lang.Symbol.R_map.t

  val sanitise :
       ?passes:Act_config.Sanitiser_pass.Set.t
    -> ?symbols:Lang.Symbol.t list
    -> Lang.Program.t
    -> Output.t Or_error.t
  (** [sanitise ?passes ?symbols stms] sanitises a statement list [stms],
      returning one or more program lists with various litmus-unfriendly
      elements removed or simplified.

      If the implementation of the sanitiser outputs multiple programs,
      [stms] will be split along its program boundaries first.

      Optional argument 'passes' controls the sanitisation passes used; the
      default is [Pass.all].

      Optional argument 'symbols' gives a list of concrete symbols that the
      sanitiser should track throughout the sanitisation process. *)
end
