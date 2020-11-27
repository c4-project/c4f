(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Sexplib

module Make (B : Loadable_types.Basic) : Loadable_types.S with type t = B.t =
struct
  include B

  let load (is : Input.t) : t Or_error.t =
    Input.with_input is ~f:(fun ic ->
        load_from_ic ?path:(Input.to_string_opt is) ic)
end

module Make_chain
    (B : Loadable_types.Basic)
    (C : Convert_types.S_with_failure with type src := B.t) :
  Loadable_types.S with type t = C.dst = Make (struct
  type t = C.dst

  let load_from_string str = Or_error.(str |> B.load_from_string >>= C.f)

  let load_from_ic ?path ic = Or_error.(B.load_from_ic ?path ic >>= C.f)
end)

let wrap (name : string) (f : unit -> 't) : 't Or_error.t =
  Or_error.tag_arg (Or_error.try_with f) "While reading from" name
    [%sexp_of: string]

module Of_sexpable (B : Sexpable.S) : Loadable_types.S with type t = B.t =
Make (struct
  type t = B.t

  let load_from_string (s : string) : t Or_error.t =
    wrap "string" (fun () -> Sexp.of_string_conv_exn s B.t_of_sexp)

  let load_from_ic ?(path = "stdin") (ic : Stdio.In_channel.t) : t Or_error.t
      =
    wrap path (fun () -> B.t_of_sexp (Sexp.input_sexp ic))
end)

module Of_jsonable (B : Jsonable_types.Of) :
  Loadable_types.S with type t = B.t = Make (struct
  type t = B.t

  let load_from_string (s : string) : t Or_error.t =
    wrap "string" (fun () -> B.t_of_yojson (Yojson.Safe.from_string s))

  let load_from_ic ?(path = "stdin") (ic : Stdio.In_channel.t) : t Or_error.t
      =
    try
      Or_error.return
        (B.t_of_yojson (Yojson.Safe.from_channel ~fname:path ic))
    with Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (exn, t) ->
      Or_error.error_s
        [%message
          "Could not parse JSON" ~path ~error:(Exn.to_string exn)
            ~json_fragment:(Yojson.Safe.to_string t)]
end)
