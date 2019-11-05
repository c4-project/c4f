(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core

module Standard_asm = struct
  type t =
    { rest: Common_cmd.Args.Standard.t Common_cmd.Args.With_files.t
    ; aux_file: string option
    ; target: Common_cmd.Asm_target.t
    ; sanitiser_passes: Act_sanitiser.Pass_group.Selector.t Blang.t option }
  [@@deriving fields]

  let get =
    Command.Let_syntax.(
      let%map target = Common_cmd.Args.asm_target
      and aux_file = Common_cmd.Args.aux_file
      and sanitiser_passes = Common_cmd.Args.sanitiser_passes
      and rest = Common_cmd.Args.(With_files.get Standard.get) in
      {rest; target; aux_file; sanitiser_passes})
end
