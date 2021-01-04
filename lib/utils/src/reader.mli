(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Basic reader monads. *)

open Base

(** Transforms [M] to the reader monad. *)
module On_monad (M : Monad.S) : Reader_types.S2 with type 'r Inner.t = 'r M.t

(** Transforms the error monad to the reader monad. *)
module With_errors : Reader_types.S2 with type 'r Inner.t = 'r Or_error.t

(** Fixes the context of an arity-2 reader monad. *)
module Fix_context (R : Reader_types.S2) (B : T) :
  Reader_types.S
    with type 'r t = ('r, B.t) R.t
     and type 'r Inner.t = 'r R.Inner.t
     and type ctx = B.t

(** We can use the reader monad without transforming an underlying monad. *)
include Reader_types.S2 with type 'r Inner.t = 'r
