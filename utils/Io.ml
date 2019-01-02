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

open Core

include Io_intf

module Dir = struct
  let default_sort_compare = Core_extended.Extended_string.collate

  let filter_files ?ext farray =
    Option.value_map
      ~default:farray
      ~f:(fun ext ->
          Array.filter ~f:(My_filename.has_extension ~ext) farray)
      ext
  ;;

  let test_files = [| "stdio.h"; "conio.h"; "main.c"; "main.o"; "README" |]

  let%expect_test "filter_files: no filter" =
    let result = filter_files test_files in
    Sexp.output_hum Out_channel.stdout [%sexp (result : string array) ];
    [%expect {| (stdio.h conio.h main.c main.o README) |}]
  ;;

  let%expect_test "filter_files: filter" =
    let result = filter_files ~ext:"c" test_files in
    Sexp.output_hum Out_channel.stdout [%sexp (result : string array) ];
    [%expect {| (main.c) |}]
  ;;

  let readdir path =
    Or_error.(
      tag_arg
        (try_with (fun () -> Sys.readdir path))
        "Couldn't read directory"
        path
        [%sexp_of: string]
    )
  ;;

  let get_files ?(compare=default_sort_compare) ?ext path =
    let open Or_error.Let_syntax in
    let%map farray = readdir path in
    let with_ext = filter_files ?ext farray in
    Array.sort ~compare with_ext;
    Array.to_list with_ext
end

module In_source = struct
  type t =
    | File of string
    | Stdin
  [@@deriving sexp, variants]
  ;;

  let to_string : t -> string = function
    | File s -> s
    | Stdin  -> "(stdin)"
  ;;

  let pp : t Fmt.t = Fmt.of_to_string to_string

  let of_option : string option -> t =
    Option.value_map ~f:file ~default:stdin
  ;;

  let to_file : t -> string option = function
    | File f -> Some f
    | Stdin  -> None
  ;;

  let with_input (src : t) ~f =
    Or_error.(
      match src with
      | File s ->
        tag_arg
          (try_with_join (fun _ -> In_channel.with_file s ~f:(f src)))
          "While reading from file:"
          s
          [%sexp_of: string]
      | Stdin ->
        tag ~tag:"While reading from standard input:"
          (try_with_join (fun _ -> f src In_channel.stdin))
    )
  ;;
end

module Out_sink = struct
  type t =
    | File of string
    | Stdout
    | Temp of { prefix : string
              ; ext    : string
              }
  [@@deriving sexp, variants]
  ;;

  let to_string : t -> string = function
    | File s -> s
    | Stdout -> "(stdout)"
    | Temp _ -> "(temp)"
  ;;

  let pp : t Fmt.t = Fmt.of_to_string to_string

  let of_option : string option -> t =
    Option.value_map ~f:file ~default:stdout
  ;;

  let to_file = function
    | File f -> Some f
    | Stdout -> None
    | Temp _ -> None
  ;;

  let with_file_output f filename =
    let open Or_error in
    let open Or_error.Let_syntax in
    let%map result =
      tag_arg
        (try_with_join (fun _ -> Out_channel.with_file filename ~f))
        "While writing to file:"
        filename
        [%sexp_of: string]
    in (Some filename, result)
  ;;

  let with_stdout_output f =
    let open Or_error in
    let open Or_error.Let_syntax in
    let%map result = try_with_join (fun _ -> f Out_channel.stdout)
    in (None, result)
  ;;

  let with_output (snk : t) ~f : (string option * 'a) Or_error.t =
    let fs = f snk in
    match snk with
    | File s -> with_file_output fs s
    | Stdout -> with_stdout_output fs
    | Temp { prefix; ext } ->
      with_file_output fs (Filename.temp_file prefix ext)
  ;;
end

let with_input_and_output src snk ~f =
  In_source.with_input src
    ~f:(fun isrc' ic -> Out_sink.with_output snk ~f:(f isrc' ic))
;;

let print_bool = Out_channel.printf "%b"
