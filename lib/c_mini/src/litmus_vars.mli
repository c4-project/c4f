(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Generic support for extracting variable information from C litmus tests.

    These functions all expect the incoming tests to have C functions that
    express their dependencies on global variables in the form of pointer
    parameters. This encoding is how Memalloy, at least, emits global
    variable dependencies. Each function will fail in situations where this
    assumption doesn't hold (for example, where the input list is empty, or
    the C functions disagree on their parameters). *)

open Base

val make_scoped_map :
     Litmus.Test.t
  -> make_global:(Act_common.C_id.t -> Type.t -> 'a)
  -> make_local:(int -> Act_common.C_id.t -> Type.t -> 'a)
  -> 'a Act_common.Scoped_map.t Or_error.t
(** [make_scoped_map vast ~make_global ~make_local] tries to make a scoped
    map by inspecting all local variable declarations and
    global-representing parameters in the functions in [vast]. It uses
    [make_global] and [make_local] to construct the values stored in the
    scoped map. *)

val make_set : Litmus.Test.t -> Set.M(Act_common.Litmus_id).t Or_error.t
(** [make_set vast] tries to make a Litmus ID set by inspecting all local
    variable declarations and global-representing parameters in the
    functions in [fns]. *)
