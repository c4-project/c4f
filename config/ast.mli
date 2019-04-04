(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to permit
   persons to whom the Software is furnished to do so, subject to the
   following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
   NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
   DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
   OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
   USE OR OTHER DEALINGS IN THE SOFTWARE. *)

(** Abstract syntax tree for config files

    The config {{!Config_parser} parser} emits this AST; later on, the
    {{!Config.File} config module proper} uses it to build a raw config. *)

(** Items in a C-preprocessor stanza *)
module Cpp : sig
  type t = Cmd of string | Argv of string list | Enabled of bool
end

(** Items in a fuzzer stanza *)
module Fuzz : sig
  type t = Action of Id.t * int option
end

(** Items in a Litmus stanza *)
module Litmus : sig
  type t = Cmd of string
end

(** Items in a Herd stanza *)
module Herd : sig
  type t = Cmd of string | C_model of string | Asm_model of Id.t * string
end

(** Items in a SSH stanza *)
module Ssh : sig
  type t = User of string | Host of string | Copy_to of string
end

(** Items in a via stanza *)
module Via : sig
  type t = Local | Ssh of Ssh.t list
end

(** Items in a machine stanza *)
module Machine : sig
  (** Type of machine stanza items. *)
  type t =
    | Enabled of bool  (** Whether or not the machine is enabled. *)
    | Via of Via.t  (** Where the machine is located. *)
    | Litmus of Litmus.t list
        (** Information about how to run `litmus` on this machine. *)
end

(** Items in a compiler stanza *)
module Compiler : sig
  type t =
    | Enabled of bool
    | Style of Id.t
    | Emits of Id.t
    | Cmd of string
    | Argv of string list
    | Herd of bool
    | Machine of Id.t
end

(** Items at the top level *)
module Top : sig
  type t =
    | Cpp of Cpp.t list  (** A C preprocessor config block. *)
    | Fuzz of Fuzz.t list  (** A fuzzer config block. *)
    | Herd of Herd.t list  (** A Herd config block. *)
    | Machine of Id.t * Machine.t list  (** A machine config block. *)
    | Compiler of Id.t * Compiler.t list  (** A compiler config block. *)
end

(** A config AST is a list of top-level items. *)
type t = Top.t list
