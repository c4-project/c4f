(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

val token : Set.M(String).t -> Sedlexing.lexbuf -> Parser.token
(** [token typedefs lexbuf] lexes the next token in [lexbuf]. It supports
    the C 'lexer hack' by taking in a set of known typedefs [typedefs]. *)
