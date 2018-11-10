(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation
   files (the "Software"), to deal in the Software without
   restriction, including without limitation the rights to use, copy,
   modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

(** [Loadable] contains signatures for abstract data types that can
    be loaded from a file or string, and functors for adding
    convenience functions to such types for loading from a variety of
    sources. *)

open Core

module type Basic = sig
  type t
  val load_from_string : string -> t Or_error.t;;
  val load_from_ic
    :  ?path:string
    -> In_channel.t
    -> t Or_error.t;;
end

module type S = sig
  include Basic
  val load_from_isrc : Io.In_source.t -> t Or_error.t;;
  val load : path:string -> t Or_error.t;;
end

module Make (B : Basic) : S with type t := B.t = struct
  include B

  let load_from_isrc =
    Io.In_source.with_input
      ~f:(fun is ic -> load_from_ic ?path:(Io.In_source.file is) ic)
  ;;

  let load ~path = load_from_isrc (`File path);;
end

module Of_sexpable (B : Sexpable.S) : S with type t := B.t
  = Make (struct
    type t = B.t

    let wrap name f =
      Or_error.tag_arg
        (Or_error.try_with f)
        "While reading from"
        name
        [%sexp_of: string]
    ;;

    let load_from_string s =
      wrap "string" (fun () -> Sexp.of_string_conv_exn s B.t_of_sexp)
    ;;

    let load_from_ic ?(path="stdin") ic =
      wrap path (fun () -> B.t_of_sexp (Sexp.input_sexp ic))
    ;;
  end)
;;
