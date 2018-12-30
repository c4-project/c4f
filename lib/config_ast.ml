(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation
   files (the "Software"), to deal in the Software without
   restriction, including without limitation the rights to use, copy,
   modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

module Cpp = struct
  type t =
    | Cmd of string
    | Enabled of bool
  ;;
end

module Herd = struct
  type t =
    | Cmd of string
    | C_model of string
    | Asm_model of Id.t * string
  ;;
end

module Ssh = struct
  type t =
    | User of string
    | Host of string
    | Copy_to of string
  ;;
end

module Via = struct
  type t =
    | Local
    | Ssh of Ssh.t list
  ;;
end

module Machine = struct
  type t =
    | Enabled of bool
    | Via of Via.t
  ;;
end

module Compiler = struct
  type t =
    | Enabled of bool
    | Style   of Id.t
    | Emits   of Id.t
    | Cmd     of string
    | Argv    of string list
    | Herd    of bool
    | Machine of Id.t
  ;;
end

module Top = struct
  type t =
    | Cpp of Cpp.t list
    | Herd of Herd.t list
    | Machine of Id.t * Machine.t list
    | Compiler of Id.t * Compiler.t list
  ;;
end

type t = Top.t list
