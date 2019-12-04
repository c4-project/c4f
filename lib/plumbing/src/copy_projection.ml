(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

type t = {local: Fpath.t; remote: string} [@@deriving fields, make]

let project (spec : Fpath.t Copy_spec.t)
    ~(f : [`Directory | `File] -> Fpath.t -> string) : t Copy_spec.t =
  Copy_spec.map_with_kind spec ~f:(fun kind local ->
      make ~local ~remote:(f kind local))

let project_back (spec : string Copy_spec.t)
    ~(f : [`Directory | `File] -> string -> Fpath.t) : t Copy_spec.t =
  Copy_spec.map_with_kind spec ~f:(fun kind remote ->
      make ~local:(f kind remote) ~remote)

let all_local (spec : t Copy_spec.t) : Fpath.t Copy_spec.t =
  Copy_spec.map ~f:local spec

let all_remote (spec : t Copy_spec.t) : string Copy_spec.t =
  Copy_spec.map ~f:remote spec

let try_find (spec : t Copy_spec.t) (looking_for : Fpath.t) : string option =
  List.find_map (Copy_spec.paths spec) ~f:(fun {local; remote} ->
      Option.some_if (Fpath.equal local looking_for) remote)

let all_remote_pair ({input; output} : t Copy_spec.Pair.t) :
    string Copy_spec.Pair.t =
  {input= all_remote input; output= all_remote output}
