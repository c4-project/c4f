(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(* {1 Test helpers for statements} *)

val mkif :
     ?cond:Act_fir.Expression.t
  -> unit Act_fir.Statement.t list
  -> unit Act_fir.Statement.t list
  -> unit Act_fir.Statement.t
(** [mkif ?cond ts fs] is a convenience constructor for if statements with
    true statements [ts] and false statements [fs]. *)

val mkwhile :
     ?cond:Act_fir.Expression.t
  -> unit Act_fir.Statement.t list
  -> unit Act_fir.Statement.t
(** [mkwhile ?cond xs fs] is a convenience constructor for while loops with
    statements [xs]. *)

val nop : unit Act_fir.Statement.t
(** [nop] is a no-operation statement. *)
