(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module A = Act_common

type t = Act_common.Id.t Act_machine.Target.t

let resolve (target : t) ~(cfg : Act_config.Act.t) :
    Act_machine.Qualified.Compiler.t Act_machine.Target.t Or_error.t =
  Act_machine.Target.With_errors.map_left_m
    ~f:(fun fqid -> Act_config.Act.compiler cfg ~fqid)
    target

