(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the "Software"),
   to deal in the Software without restriction, including without limitation
   the rights to use, copy, modify, merge, publish, distribute, sublicense,
   and/or sell copies of the Software, and to permit persons to whom the
   Software is furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
   THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
   DEALINGS IN THE SOFTWARE. *)

(** High-level module for emitting act's internal assembly analysis

    [Explainer] contains functors for extracting a pretty-printable summary
    of an assembly listing as act understands it, through a language module. *)

module Config : sig
  module Format : sig
    (** [t] is an enumeration of output formats for explain jobs. *)
    type t =
      | Assembly
          (** Terse, but as close to parseable assembly as possible *)
      | Detailed
          (** More details than [Assembly], but verbose and free-form *)
    [@@deriving equal]

    val default : t
    (** [default] gets the default output format. *)
  end

  type t [@@deriving equal]

  val make : ?format:Format.t -> ?aux:Act_delitmus.Aux.t -> unit -> t
  (** [make ?format ?aux ()] builds an [Explain_config] with the given
      parameters. *)

  val default : t
  (** [default] gets the default explainer job configuration. *)
end

(** {2 Module type synonyms} *)

module type S = Explainer_intf.S with type config := Config.t

module type S_filter = Runner_intf.S with type cfg = Config.t

(** {2 Making an explainer} *)

(** [Make] makes an implementation of [S] for a given language. *)
module Make (B : Explainer_intf.Basic) : S with module Lang = B.Src_lang
