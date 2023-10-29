(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

module Adjusted = struct
  type 'v t = Not_adjusted of 'v | Adjusted of {original: 'v; actual: 'v}

  let make (user : 'v option) ~(default : 'v) : 'v t =
    Option.value_map user
      ~f:(fun actual -> Adjusted {original= default; actual})
      ~default:(Not_adjusted default)

  let pp (pp_v : 'v Fmt.t) (f : Formatter.t) : 'v t -> unit = function
    | Not_adjusted o -> pp_v f o
    | Adjusted {original; actual} ->
        Fmt.pf f "%a (normally %a)" pp_v actual pp_v original
end

type 'v t = {value: 'v Adjusted.t; readme: string} [@@deriving fields]

let pp (pp_v : 'v Fmt.t) (vname : string) (f : Formatter.t)
    ({value; readme} : 'v t) : unit =
  Fmt.pf f "@[<v>@[%s:@ %a@]@,@[<hv 2>Summary:@ @[%a@]@]@]" vname
    (Adjusted.pp pp_v) value Fmt.paragraphs readme

let pp_map (pp_v : 'v Fmt.t) (vname : string) : 'v t Map.M(Common.Id).t Fmt.t
    =
  Common.Id.pp_map (pp pp_v vname)

let pp_map_terse (pp_v : 'v Fmt.t) : 'v t Map.M(Common.Id).t Fmt.t =
  Fmt.(
    using Map.to_alist
      (list ~sep:sp
         (hbox
            (pair ~sep:(any ":@ ") Common.Id.pp
               (using (fun x -> x.value) (Adjusted.pp pp_v)) ) ) ) )
