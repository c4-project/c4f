(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Functions for reifying a FIR program into an AST.

    These functions reify various top-level elements of a FIR program. Other
    modules govern smaller components:

    - {!Reify_atomic} for atomics;
    - {!Reify_prim} for miscellaneous primitives;
    - {!Reify_expr} for expressions;
    - {!Reify_stm} for statements. *)

val func : _ C4f_fir.Function.t C4f_common.C_named.t -> Ast.External_decl.t
(** [func f] reifies the FIR function [f] into the C AST. *)

val program : _ C4f_fir.Program.t -> Ast.Translation_unit.t
(** [program p] reifies the FIR program [p] into the C AST. *)

(** {1 Pretty printers using reification} *)

val pp_func : _ C4f_fir.Function.t C4f_common.C_named.t Fmt.t
(** [pp_func] pretty-prints FIR functions via {!func}. *)

val pp : _ C4f_fir.Program.t Fmt.t
(** [pp] pretty-prints FIR programs via {!program}. *)

(** {2 Litmus tests} *)

val pp_litmus_raw : C4f_litmus.Test.Raw.M(C4f_fir.Litmus.Lang).t Fmt.t
(** [pp_litmus_raw] pretty-prints unverified FIR litmus tests. *)

val pp_litmus : C4f_fir.Litmus.Test.t Fmt.t
(** [pp_litmus] pretty-prints verified FIR litmus tests. *)
