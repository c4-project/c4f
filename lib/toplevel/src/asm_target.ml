(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module A = Act_common

type t = Arch of A.Id.t | Compiler_id of A.Id.t [@@deriving variants]

let resolve_compiler (cfg : Act_config.Act.t) (fqid : A.Id.t) :
    Act_machine.Target.t Or_error.t =
  Or_error.Let_syntax.(
    let%map spec = Act_config.Act.compiler cfg ~fqid in
    `Spec spec)

let resolve (target : t) ~(cfg : Act_config.Act.t) :
    Act_machine.Target.t Or_error.t =
  Variants.map
    ~compiler_id:(fun _ -> resolve_compiler cfg)
    ~arch:(fun _ arch -> Or_error.return (`Arch arch))
    target
