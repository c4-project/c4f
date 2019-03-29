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

open Core
include Io_intf

let fpath_of_string (s : string) : Fpath.t Or_error.t =
  Result.map_error (Fpath.of_string s) ~f:(function `Msg s ->
      Error.of_string s )

let lift_fpath_str (f : Fpath.t -> 'a) (default : 'a) :
    string option -> 'a Or_error.t = function
  | None ->
      Or_error.return default
  | Some s ->
      Or_error.(s |> fpath_of_string >>| f)

let fpath_of_string_option : string option -> Fpath.t option Or_error.t =
  lift_fpath_str Option.some None

let filename_no_ext (f : Fpath.t) : string =
  Fpath.(filename (rem_ext ~multi:true f))

let%expect_test "filename_no_ext: example" =
  printf "%s\n" (filename_no_ext Fpath.(v "foo" / "bar" / "baz.c")) ;
  [%expect {| baz |}]

let%expect_test "filename_no_ext: example with double extension" =
  printf "%s\n" (filename_no_ext Fpath.(v "foo" / "bar" / "baz.c.litmus")) ;
  [%expect {| baz |}]

module In_source = struct
  type t = File of Fpath.t | Stdin of {file_type: string option}
  [@@deriving variants]

  (* overrides to lift options into optional arguments *)
  let stdin ?file_type () : t = stdin ~file_type

  let file_type : t -> string option = function
    | File fp ->
        Option.some_if (Fpath.exists_ext fp)
          (String.lstrip ~drop:(Char.equal '.') (Fpath.get_ext fp))
    | Stdin sd ->
        sd.file_type

  let%expect_test "file_type: file with two extensions" =
    Fmt.(pr "%a@." (option string))
      (file_type (file (Fpath.v "iriw.c.litmus"))) ;
    [%expect {| litmus |}]

  let%expect_test "file_type: stdin with specific type" =
    Fmt.(pr "%a@." (option string))
      (file_type (stdin ~file_type:"litmus" ())) ;
    [%expect {| litmus |}]

  let to_string : t -> string = function
    | File s ->
        Fpath.to_string s
    | Stdin _ ->
        "(stdin)"

  let pp : t Fmt.t = Fmt.of_to_string to_string

  let of_fpath : Fpath.t -> t = file

  let of_fpath_opt : Fpath.t option -> t =
    Option.value_map ~f:file ~default:(stdin ())

  let of_string_opt : string option -> t Or_error.t =
    lift_fpath_str file (stdin ())

  let to_file : t -> Fpath.t option = function
    | File f ->
        Some f
    | Stdin _ ->
        None

  let to_file_err (src : t) : Fpath.t Or_error.t =
    Result.of_option (to_file src)
      ~error:(Error.createf "Must read from a file, got %s" (to_string src))

  let with_input (src : t) ~f =
    Or_error.(
      match src with
      | File file ->
          let s = Fpath.to_string file in
          tag_arg
            (try_with_join (fun _ -> In_channel.with_file s ~f:(f src)))
            "While reading from file:" s [%sexp_of: string]
      | Stdin _ ->
          tag ~tag:"While reading from standard input:"
            (try_with_join (fun _ -> f src In_channel.stdin)))
end

module Out_sink = struct
  type t = File of Fpath.t | Stdout [@@deriving variants]

  let temp ~(prefix : string) ~(ext : string) : t =
    file (Fpath.v (Filename.temp_file prefix ("." ^ ext)))

  let to_string : t -> string = function
    | File s ->
        Fpath.to_string s
    | Stdout ->
        "(stdout)"

  let as_in_source : t -> In_source.t Or_error.t = function
    | File f ->
        Or_error.return (In_source.file f)
    | x ->
        Or_error.errorf "Can't use %s as an input source" (to_string x)

  let pp : t Fmt.t = Fmt.of_to_string to_string

  let of_fpath : Fpath.t -> t = file

  let of_fpath_opt : Fpath.t option -> t =
    Option.value_map ~f:file ~default:stdout

  let of_string_opt : string option -> t Or_error.t =
    lift_fpath_str of_fpath stdout

  let to_file : t -> Fpath.t option = function
    | File f ->
        Some f
    | Stdout ->
        None

  let to_file_err (src : t) : Fpath.t Or_error.t =
    Result.of_option (to_file src)
      ~error:(Error.createf "Must write to a file, got %s" (to_string src))

  let with_file_output (fpath : Fpath.t) f =
    let fpath_raw = Fpath.to_string fpath in
    Or_error.(
      tag_arg
        (try_with_join (fun _ -> Out_channel.with_file fpath_raw ~f))
        "While writing to file:" fpath_raw [%sexp_of: string])

  let with_stdout_output f =
    Or_error.try_with_join (fun _ -> f Out_channel.stdout)

  let with_output (snk : t) ~f : 'a Or_error.t =
    ( match snk with
    | File fpath ->
        with_file_output fpath
    | Stdout ->
        with_stdout_output )
      (f snk)
end

let with_input_and_output (src : In_source.t) (snk : Out_sink.t) ~f :
    'a Or_error.t =
  In_source.with_input src ~f:(fun isrc' ic ->
      Out_sink.with_output snk ~f:(f isrc' ic) )

let print_bool = Out_channel.printf "%b"
