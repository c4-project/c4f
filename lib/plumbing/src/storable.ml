(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Sexplib

module Make (B : Storable_types.Basic) : Storable_types.S with type t = B.t =
struct
  include B

  let store (x : t) ~(dest : Output.t) : unit Or_error.t =
    Output.with_output dest ~f:(fun oc ->
        store_to_oc x ?path:(Output.to_string_opt dest) ~dest:oc)
end

module Make_chain
    (B : Storable_types.Basic)
    (C : Convert_types.S_with_failure with type dst := B.t) :
  Storable_types.S with type t = C.src = Make (struct
  type t = C.src

  let store_to_oc ?(path : string option) (x : C.src)
      ~(dest : Stdio.Out_channel.t) : unit Or_error.t =
    Or_error.(x |> C.f >>= B.store_to_oc ?path ~dest)
end)

let wrap (name : string) (f : unit -> 't) : 't Or_error.t =
  Or_error.tag_arg (Or_error.try_with f) "While writing to" name
    [%sexp_of: string]

module Of_sexpable (B : Sexpable.S) : Storable_types.S with type t = B.t =
Make (struct
  type t = B.t

  let store_to_oc ?(path = "stdout") (x : t) ~(dest : Stdio.Out_channel.t) :
      unit Or_error.t =
    wrap path (fun () -> Sexp.output_hum dest (B.sexp_of_t x))
end)

module Of_jsonable (B : Jsonable_types.To) :
  Storable_types.S with type t = B.t = Make (struct
  type t = B.t

  let store_to_oc ?(path = "stdout") (x : t) ~(dest : Stdio.Out_channel.t) :
      unit Or_error.t =
    wrap path (fun () ->
        Yojson.Safe.pretty_to_channel dest (B.yojson_of_t x))
end)
