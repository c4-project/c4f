(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(* {1 Test helpers for statements}

   These functions all generate particular types of statements useful in
   tests, with no metadata attached. *)

val mkif :
     ?cond:C4f_fir.Expression.t
  -> unit C4f_fir.Statement.t list
  -> unit C4f_fir.Statement.t list
  -> unit C4f_fir.Statement.t
(** [mkif ?cond ts fs] is a convenience constructor for if statements with
    true statements [ts] and false statements [fs]. *)

val mkwhile :
     ?cond:C4f_fir.Expression.t
  -> unit C4f_fir.Statement.t list
  -> unit C4f_fir.Statement.t
(** [mkwhile ?cond xs fs] is a convenience constructor for while loops with
    statements [xs]. *)

val nop : unit C4f_fir.Statement.t
(** [nop] is a no-operation statement. *)

(** {2 Atomic action shorthand} *)

val mkaxchg :
     ?mo:C4f_fir.Mem_order.t
  -> C4f_fir.Address.t
  -> C4f_fir.Expression.t
  -> unit C4f_fir.Statement.t
(** [mkaxchg ?mo obj desired] is an atomic exchange statement with the given
    [obj], [desired], and optional [mo] (defaults to sequential consistency). *)

val mkastore :
     ?mo:C4f_fir.Mem_order.t
  -> C4f_fir.Address.t
  -> C4f_fir.Expression.t
  -> unit C4f_fir.Statement.t
(** [mkastore ?mo dst src] is an atomic store statement with the given [dst],
    [src], and optional [mo] (defaults to sequential consistency). *)

val mkafetch :
     ?mo:C4f_fir.Mem_order.t
  -> C4f_fir.Op.Fetch.t
  -> C4f_fir.Address.t
  -> C4f_fir.Expression.t
  -> unit C4f_fir.Statement.t
(** [mkastore ?mo op obj arg] is an atomic store statement with the given
    [dst], [src], [op], and optional [mo] (defaults to sequential
    consistency). *)
