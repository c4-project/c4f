(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core

(** The license information.

    This should ideally match the header comment above it. *)
let license : string Lazy.t =
  lazy
    (C4f_utils.My_string.format_for_readme
       {| This file is part of c4f.

         Copyright (c) 2018-2022 C4 Project

         c4t itself is licensed under the MIT License. See the LICENSE file in the
         project root for more information.

         Parts of c4t are based on code from the Herdtools7 project
         (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
         project root for more information. |} )

let command : Command.t =
  Command.basic ~summary:"Prints license information"
    Command.Let_syntax.(
      let%map_open _ = Args.Standard.get in
      fun () -> Printf.printf "%s" (Lazy.force license))
