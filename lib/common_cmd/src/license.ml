(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core_kernel

(** The license information.

    This should ideally match the header comment above it. *)
let license : string Lazy.t =
  lazy
    (Act_utils.My_string.format_for_readme
       {| The Automagic Compiler Tormentor

         Copyright (c) 2018--2019 Matt Windsor and contributors

         ACT itself is licensed under the MIT License. See the LICENSE file in the
         project root for more information.

         ACT is based in part on code from the Herdtools7 project
         (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
         project root for more information. |})

let command : Command.t =
  Command.basic ~summary:"Prints license information"
    Command.Let_syntax.(
      let%map_open _ = Args.Standard.get in
      fun () -> Printf.printf "%s" (Lazy.force license))
