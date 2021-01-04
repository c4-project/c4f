(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Delitmus: main drivers.

    These modules actually 'do' the delitmusifying process: they take a
    validated C litmus AST and output a pair of C program and auxiliary
    output file. *)

(** Makes a delitmus driver given a function rewriter, information about how
    global and local variables are mapped, and various other configuration
    tidbits. *)
module Make (Basic : Runner_types.Basic) : Runner_types.S
