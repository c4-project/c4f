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

open Base
open Sexplib
open Loadable_intf

module Make (B : Basic) : S with type t = B.t = struct
  include B

  let path_of_is (is : Plumbing.Input.t) : string option =
    is |> Plumbing.Input.to_file |> Option.map ~f:Fpath.to_string

  let load_from_isrc is =
    Plumbing.Input.with_input is ~f:(fun ic ->
        load_from_ic ?path:(path_of_is is) ic )

  let load ~path = load_from_isrc (Plumbing.Input.file path)
end

module Make_chain (B : Basic) (C : Basic_chain with type src := B.t) :
  S with type t = C.dst = Make (struct
  type t = C.dst

  let load_from_string str = Or_error.(str |> B.load_from_string >>= C.f)

  let load_from_ic ?path ic = Or_error.(B.load_from_ic ?path ic >>= C.f)
end)

module Of_sexpable (B : Sexpable.S) : S with type t = B.t = Make (struct
  type t = B.t

  let wrap name f =
    Or_error.tag_arg (Or_error.try_with f) "While reading from" name
      [%sexp_of: string]

  let load_from_string s =
    wrap "string" (fun () -> Sexp.of_string_conv_exn s B.t_of_sexp)

  let load_from_ic ?(path = "stdin") ic =
    wrap path (fun () -> B.t_of_sexp (Sexp.input_sexp ic))
end)

module To_filter (L : S) :
  Plumbing.Filter.S with type aux_i = unit and type aux_o = L.t =
Plumbing.Filter.Make (struct
  type aux_i = unit

  type aux_o = L.t

  let name = "(loading)"

  let tmp_file_ext = Fn.const "tmp"

  let run _ctx ic _oc = L.load_from_ic ic
end)
