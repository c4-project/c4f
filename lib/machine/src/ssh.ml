(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

type t = {user: string option [@sexp.option]; host: string; copy_dir: string}
[@@deriving sexp, make, fields, equal]

let pp f {host; user; copy_dir} =
  match user with
  | Some u ->
      Fmt.pf f "%s@@%s:%s" host u copy_dir
  | None ->
      Fmt.pf f "%s:%s" host copy_dir

module To_config (C : sig
  val ssh : t
end) : Act_utils.Ssh_types.S = struct
  let host = host C.ssh

  let user = user C.ssh
end
