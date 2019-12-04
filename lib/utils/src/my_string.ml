(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

let has_prefix (str : string) ~(prefix : string) : bool =
  Option.is_some (String.chop_prefix str ~prefix)

let ensure_prefix (str : string) ~(prefix : string) : string =
  let rest = Option.value (String.chop_prefix str ~prefix) ~default:str in
  prefix ^ rest

let has_suffix (str : string) ~(suffix : string) : bool =
  Option.is_some (String.chop_suffix str ~suffix)

let ensure_suffix (str : string) ~(suffix : string) : string =
  let rest = Option.value (String.chop_suffix str ~suffix) ~default:str in
  rest ^ suffix

let format_for_readme (str : string) : string =
  Fmt.(str "%a" (box paragraphs)) str
