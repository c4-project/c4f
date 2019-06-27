(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

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

(** Abstract program model: basic node signatures *)

open Base
open Act_utils

(** [S] is the baseline signature for all abstract observation types. *)
module type S = sig
  type t [@@deriving sexp]

  include Pretty_printer.S with type t := t

  (** [Kind] contains an enumeration of all of the possible high-level
      'kinds' of [t]. If [t] is a variant, [Kind.t] will usually be the
      result of removing all of the arguments from [t]'s constructors. *)
  module Kind : sig
    type t

    include Enum.S_table with type t := t

    include Enum.Extension_table with type t := t
  end

  module Flag : Flag_enum.S
  (** [Flag] is a (potentially unpopulated) set of additional flags that can
      be attached to an observation. *)

  val kind : t -> Kind.t
  (** [kind x] gets the underlying kind of [x]. *)
end
