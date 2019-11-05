(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

module Tree_doc = struct
  type elt = {args: string list; details: string}

  type t = (string, elt) List.Assoc.t

  let rearrange ((name, mdoc) : string * elt option) :
      (string * string list) * string =
    match mdoc with
    | Some {args; details} ->
        ((name, args), details)
    | None ->
        ((name, []), "DOC MISSING")

  let pp_header : (string * string list) Fmt.t =
    Fmt.(
      using
        (function k, [] -> (k, None) | k, v -> (k, Some v))
        (pair ~sep:nop string (option (sp ++ list ~sep:sp string))))

  let pp_maybe : (string * elt option) Fmt.t =
    Fmt.(
      vbox ~indent:2
        (using rearrange (pair ~sep:sp (hbox pp_header) (box words))))

  let name_to_doc (tree_docs : t) (name : string) : string * elt option =
    (name, List.Assoc.find tree_docs name ~equal:String.Caseless.equal)

  let pp (tree_docs : t) : string list -> unit Fmt.t =
    Fmt.(const (list ~sep:sp (using (name_to_doc tree_docs) pp_maybe)))
end
