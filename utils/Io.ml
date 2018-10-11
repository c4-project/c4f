(* This file is part of 'act'.

Copyright (c) 2018 by Matt Windsor

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. *)

(** Various input/output helpers *)

open Core

module In_source = struct
  type t =
    [ `File of string
    | `Stdin
    ] [@@deriving sexp]

  let pp f =
    function
    | `File s -> String.pp f s
    | `Stdin  -> String.pp f "(stdin)"

  let of_option = Option.value_map ~f:(fun s -> `File s) ~default:`Stdin

  let with_input ~f src =
    Or_error.(
      match src with
      | `File s ->
        tag_arg
          (try_with_join (fun _ -> In_channel.with_file s ~f:(f src)))
          "While reading from file:"
          s
          [%sexp_of: string]
      | `Stdin ->
        tag ~tag:"While reading from standard input:"
          (try_with_join (fun _ -> f src In_channel.stdin))
    )
end

module Out_sink = struct
  type t =
    [ `File of string
    | `Stdout
    ] [@@deriving sexp]

  let pp f =
    function
    | `File s -> String.pp f s
    | `Stdout -> String.pp f "(stdout)"

  let of_option = Option.value_map ~f:(fun s -> `File s) ~default:`Stdout

  let with_output ~f snk =
    Or_error.(
      match snk with
      | `File s ->
        tag_arg
          (try_with_join (fun _ -> Out_channel.with_file s ~f:(f snk)))
          "While writing to file:"
          s
          [%sexp_of: string]
      | `Stdout ->
        try_with_join (fun _ -> f snk Out_channel.stdout)
    )
end

let with_input_and_output ~f src snk =
  In_source.with_input src
    ~f:(fun isrc' ic -> Out_sink.with_output snk ~f:(f isrc' ic))
