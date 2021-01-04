(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Functions for reifying a FIR statement into an AST.

    For the big picture, see {!Reify}. *)

val reify : _ Act_fir.Statement.t -> Ast.Stm.t list
(** [reify s] reifies the mini-statement [s] into the C AST. One FIR
    statement may expand into multiple C statements. *)

val reify_compound : _ Act_fir.Statement.t list -> Ast.Compound_stm.t
(** [reify_compound xs] reifies a list [xs] of statements into a C AST
    compound statement. *)

val pp : 'meta Act_fir.Statement.t Fmt.t
(** [pp] pretty-prints statements using {!reify}. *)
