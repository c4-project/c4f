(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Utility definitions for UTF-8 Sedlex lexers. *)

open Sedlexing

val skip_c_comment : lexbuf -> unit
(** [skip_c_comment lexbuf] lexes a C-style block comment, discarding it. *)

val skip_line : lexbuf -> unit
(** [skip_c_line_comment lexbuf] lexes and discards a line. *)

val skip_string : lexbuf -> unit
(** [skip_string lexbuf] lexes the body of a double-quoted string, discarding
    it. *)

val read_char : (string -> 'a) -> lexbuf -> 'a
(** [read_char token lexbuf] lexes the body of a single-quoted C char
    literal, feeding it to the token constructor [token]. *)

val read_string : (string -> 'a) -> lexbuf -> 'a
(** [read_string token lexbuf] lexes the body of a double-quoted C string
    literal, feeding it to the token constructor [token]. *)

val escape_string : string -> string
(** [escape_string token lexbuf] escapes a string [s] using the same
    conventions as [read_string]. *)
