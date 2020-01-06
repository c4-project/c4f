(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

type 'a t = {description: string; default: 'a} [@@deriving make, fields]

let pp (pp_default : 'a Fmt.t) : 'a t Fmt.t =
  Fmt.(
    record
      [ field "Default value" default pp_default
      ; field "Description" description paragraphs ])

module Int = struct
  type nonrec t = int t

  let pp : t Fmt.t = pp Int.pp
end

module Bool = struct
  type nonrec t = Flag.t t

  let pp : t Fmt.t = pp Flag.pp
end
