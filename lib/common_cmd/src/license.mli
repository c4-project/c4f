(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

val command : Core_kernel.Command.t
(** A command that prints license information.

    This command exists in an attempt to adhere to clause 5.4 of the CeCILL-B
    license, the licence used by the bits of Herdtools7 code from which some
    parts of the ACT code derive. *)
