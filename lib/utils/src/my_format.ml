(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

let null_formatter () =
  Caml.Format.make_formatter (fun _ _ _ -> ()) (fun _ -> ())

let pp_c_braces (pi : 'a Fmt.t) : 'a Fmt.t =
  Fmt.(hvbox (hvbox ~indent:4 (any "{@ " ++ pi) ++ any "@ }"))

let pp_set (type elem cmp) (elem : elem Fmt.t) : (elem, cmp) Set.t Fmt.t =
  Fmt.(using Set.to_list (braces (box (list ~sep:comma elem))))

let pp_if (t : unit Fmt.t) (f : unit Fmt.t) : bool Fmt.t =
 fun fmt x -> (if x then t else f) fmt ()

let pp_or_error (ok : 'a Fmt.t) : 'a Or_error.t Fmt.t =
  Fmt.result ~ok ~error:Error.pp
