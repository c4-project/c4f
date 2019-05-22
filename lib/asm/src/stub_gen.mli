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

(** The GCC assembly stub generator. *)

module Config : sig
  type t

  (** {3 Constructor} *)

  val make : ?separator:string -> unit -> t
  (** [make ?separator ()] makes a stub generator config with the optional
      given [separator]. *)

  (** {3 Accessors} *)

  val separator : t -> string option
  (** [separator] gets the optional separator string that the stub generator
      will insert between programs if present. *)
end

(** {2 Module type synonyms} *)

module type S = Stub_gen_intf.S with type config := Config.t

module type S_filter = Runner_intf.S with type cfg = Config.t

(** {2 Making a stub generator} *)

module Make (B : Stub_gen_intf.Basic) : S with module Lang = B.Src_lang
