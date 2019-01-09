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

let fpath_of_string (s : string) : Fpath.t Or_error.t =
    Result.map_error (Fpath.of_string s)
      ~f:(function `Msg s -> Error.of_string s)
;;

let lift_fpath_str
    (f : Fpath.t -> 'a)
    (default : 'a) : string option -> 'a Or_error.t = function
  | None -> Or_error.return default
  | Some s -> Or_error.(s |> fpath_of_string >>| f)
;;

let fpath_of_string_option
  : string option -> Fpath.t option Or_error.t =
  lift_fpath_str Option.some None
;;

module Dir = struct
  let default_sort_compare : Fpath.t -> Fpath.t -> int =
    Travesty.T_fn.on
      Fpath.to_string
      Core_extended.Extended_string.collate
  ;;

  let filter_files ?ext (flist : Fpath.t list) =
    Option.value_map
      ~default:flist
      ~f:(fun ext -> List.filter ~f:(Fpath.has_ext ext) flist)
      ext
  ;;

  let test_files = [ "stdio.h"; "conio.h"; "main.c"; "main.o"; "README" ]

  let%expect_test "filter_files: no filter" =
    let result = filter_files (List.map ~f:Fpath.v test_files) in
    Sexp.output_hum Out_channel.stdout
      [%sexp (List.map ~f:Fpath.to_string result : string list) ];
    [%expect {| (stdio.h conio.h main.c main.o README) |}]
  ;;

  let%expect_test "filter_files: filter" =
    let result = filter_files ~ext:"c" (List.map ~f:Fpath.v test_files) in
    Sexp.output_hum Out_channel.stdout
      [%sexp (List.map ~f:Fpath.to_string result : string list) ];
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

  let map_combine (xs : 'a list) ~(f : 'a -> 'b Or_error.t)
    : 'b list Or_error.t =
    Or_error.combine_errors (List.map ~f xs)
  ;;

  let get_files ?(compare=default_sort_compare) ?ext (path : Fpath.t) =
    let open Or_error.Let_syntax in
    let%bind file_str_array = readdir (Fpath.to_string path) in
    let      file_strs      = Array.to_list file_str_array in
    let%map  files          = map_combine ~f:fpath_of_string file_strs in
    let with_ext = filter_files ?ext files in
    List.sort ~compare with_ext
end

module In_source = struct
  type t =
    | File of Fpath.t
    | Stdin
  [@@deriving variants]
  ;;

  let to_string : t -> string = function
    | File s -> Fpath.to_string s
    | Stdin  -> "(stdin)"
  ;;

  let pp : t Fmt.t = Fmt.of_to_string to_string

  let of_fpath : Fpath.t -> t = file

  let of_fpath_opt : Fpath.t option -> t =
    Option.value_map ~f:file ~default:stdin
  ;;

  let of_string_opt : string option -> t Or_error.t =
    lift_fpath_str file stdin
  ;;

  let to_file : t -> Fpath.t option = function
    | File f -> Some f
    | Stdin  -> None
  ;;

  let to_file_err (src : t) : Fpath.t Or_error.t =
    Result.of_option (to_file src)
      ~error:(Error.createf "Must read from a file, got %s" (to_string src))
  ;;

  let with_input (src : t) ~f =
    Or_error.(
      match src with
      | File file ->
        let s = Fpath.to_string file in
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
    | File of Fpath.t
    | Stdout
  [@@deriving variants]
  ;;

  let temp ~(prefix : string) ~(ext : string) : t =
    file (Fpath.v (Filename.temp_file prefix ("." ^ ext)))
  ;;

  let to_string : t -> string = function
    | File s -> Fpath.to_string s
    | Stdout -> "(stdout)"
  ;;

  let as_in_source : t -> In_source.t Or_error.t = function
    | File f -> Or_error.return (In_source.file f)
    | x ->
      Or_error.errorf "Can't use %s as an input source" (to_string x)
  ;;

  let pp : t Fmt.t = Fmt.of_to_string to_string

  let of_fpath : Fpath.t -> t = file

  let of_fpath_opt : Fpath.t option -> t =
    Option.value_map ~f:file ~default:stdout
  ;;

  let of_string_opt : string option -> t Or_error.t =
    lift_fpath_str of_fpath stdout
  ;;

  let to_file : t -> Fpath.t option = function
    | File f -> Some f
    | Stdout -> None
  ;;

  let to_file_err (src : t) : Fpath.t Or_error.t =
    Result.of_option (to_file src)
      ~error:(Error.createf "Must write to a file, got %s" (to_string src))
  ;;

  let with_file_output f (fpath : Fpath.t) =
    let fpath_raw = Fpath.to_string fpath in
    Or_error.(
      tag_arg
        (try_with_join (fun _ -> Out_channel.with_file fpath_raw ~f))
        "While writing to file:"
        fpath_raw
        [%sexp_of: string]
    )
  ;;

  let with_stdout_output f =
    Or_error.try_with_join (fun _ -> f Out_channel.stdout)
  ;;

  let with_output (snk : t) ~f : 'a Or_error.t =
    let fs = f snk in
    match snk with
    | File fpath -> with_file_output fs fpath
    | Stdout -> with_stdout_output fs
  ;;
end

let with_input_and_output
    (src : In_source.t) (snk : Out_sink.t)
    ~f
  : 'a Or_error.t =
  In_source.with_input src
    ~f:(fun isrc' ic -> Out_sink.with_output snk ~f:(f isrc' ic))
;;

let print_bool = Out_channel.printf "%b"
