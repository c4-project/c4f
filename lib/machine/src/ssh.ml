(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

type t = {remote: Plumbing.Ssh.t; copy_dir: string}
[@@deriving sexp, make, fields, equal]

let host (ssh : t) : string = Plumbing.Ssh.host (remote ssh)

let user (ssh : t) : string option = Plumbing.Ssh.user (remote ssh)

let pp : t Fmt.t =
  Fmt.(
    using user (option (string ++ any "@@"))
    ++ using host string ++ any ":" ++ using copy_dir string)

module To_config (C : sig
  val ssh : t
end) : Plumbing.Ssh_types.S = Plumbing.Ssh.Make (struct
  let ssh = remote C.ssh
end)
