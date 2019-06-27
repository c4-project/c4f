(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Au = Act_utils
module Ac = Act_common
module Pb = Plumbing

type t = Local | Ssh of Ssh.t [@@deriving sexp, variants, equal]

let pp f = function Local -> String.pp f "local" | Ssh s -> Ssh.pp f s

let to_runner = function
  | Local ->
      (module Pb.Runner.Local : Pb.Runner_types.S)
  | Ssh c ->
      ( module Au.Ssh.Runner (struct
        include Ssh.To_config (struct
          let ssh = c
        end)

        let remote_dir = Fn.const (Ssh.copy_dir c)
      end) : Pb.Runner_types.S )

let remoteness = function
  | Local ->
      `Local
  (* Technically, if we're SSHing to loopback, this isn't true, but I
     suspect it doesn't matter. *)
  | Ssh _ ->
      `Remote
