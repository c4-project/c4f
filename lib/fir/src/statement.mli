(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** FIR: top-level statements. *)

module I := If
module W := While

(** A statement.

    We treat some things that are technically expressions in C as statements,
    for simplicity. *)
module rec Main :
  (Statement_types.S_statement
    with type 'meta if_stm := 'meta If.t
     and type 'meta while_loop := 'meta While.t)

and If :
  (Statement_types.S_if_statement
    with type 'meta expr := Expression.t
     and type 'meta stm := 'meta Main.t)

and While :
  (Statement_types.S_while_loop
    with type 'meta expr := Expression.t
     and type 'meta stm := 'meta Main.t)

include module type of Main with type 'meta t = 'meta Main.t

(** {1 Derived operations} *)

val reduce :
     'meta t
  -> prim:('meta * Prim_statement.t -> 'result)
  -> if_stm:(('meta, 'result) I.t -> 'result)
  -> while_loop:(('meta, 'result) W.t -> 'result)
  -> 'result
(** [reduce stm ~prim ~if_stm ~while_loop] applies the appropriate function
    of those given to [stm]. Unlike {!reduce_step}, it _does_ recursively
    reduce statements inside blocks. *)
