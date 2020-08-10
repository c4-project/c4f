(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Functions for reifying a FIR program into an AST.

    These functions reify various top-level elements of a FIR program. Other
    modules govern smaller components:

    - {!Reify_atomic} for atomics;
    - {!Reify_prim} for miscellaneous primitives;
    - {!Reify_expr} for expressions;
    - {!Reify_stm} for statements. *)

val func : _ Act_fir.Function.t Act_common.C_named.t -> Ast.External_decl.t
(** [func f] reifies the FIR function [f] into the C AST. *)

val program : _ Act_fir.Program.t -> Ast.Translation_unit.t
(** [program p] reifies the FIR program [p] into the C AST. *)

(** {1 Pretty printers using reification} *)

val pp_func : _ Act_fir.Function.t Act_common.C_named.t Fmt.t
(** [pp_func] pretty-prints FIR functions via {!func}. *)

val pp : _ Act_fir.Program.t Fmt.t
(** [pp] pretty-prints FIR programs via {!program}. *)

(** {2 Litmus tests} *)

val pp_litmus_raw : Act_litmus.Test.Raw.M(Act_fir.Litmus.Lang).t Fmt.t
(** [pp_litmus_raw] pretty-prints unverified FIR litmus tests. *)

val pp_litmus : Act_fir.Litmus.Test.t Fmt.t
(** [pp_litmus] pretty-prints verified FIR litmus tests. *)
