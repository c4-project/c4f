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

open Base
open Act_common

module Cpp = struct
  type t = Cmd of string | Argv of string list | Enabled of bool
  [@@deriving sexp]
end

module Fuzz = struct
  type t = Action of Id.t * int option [@@deriving sexp]
end

module Sim = struct
  type t =
    | Cmd of string
    | Style of Id.t
    | C_model of string
    | Asm_model of Id.t * string
  [@@deriving sexp]
end

module Ssh = struct
  type t = User of string | Host of string | Copy_to of string
  [@@deriving sexp]
end

module Via = struct
  type t = Local | Ssh of Ssh.t list [@@deriving sexp]
end

module Compiler = struct
  type t =
    | Enabled of bool [@sexp.bool]
    | Style of Id.t
    | Emits of Id.t
    | Cmd of string
    | Argv of string list [@sexp.list]
  [@@deriving sexp]
end

module Machine = struct
  type t =
    | Compiler of Id.t * Compiler.t list [@sexp.list]
    | Enabled of bool
    | Via of Via.t
    | Sim of Id.t * Sim.t list [@sexp.list]
  [@@deriving sexp]

  let as_compiler : t -> (Id.t * Compiler.t list) option = function
    | Compiler (i, c) ->
        Some (i, c)
    | _ ->
        None
end

module Default = struct
  module Category = struct
    type t = Arch | Compiler | Machine | Sim [@@deriving sexp]
  end

  type t = Try of Category.t * Id.t [@@deriving sexp]
end

module Top = struct
  type t =
    | Cpp of Cpp.t list [@sexp.list]
    | Default of Default.t list [@sexp.list]
    | Fuzz of Fuzz.t list [@sexp.list]
    | Machine of Id.t * Machine.t list [@sexp.list]
  [@@deriving sexp, variants]

  let as_cpp : t -> Cpp.t list option = function
    | Cpp c ->
        Some c
    | _ ->
        None

  let as_default : t -> Default.t list option = function
    | Default c ->
        Some c
    | _ ->
        None

  let as_fuzz : t -> Fuzz.t list option = function
    | Fuzz f ->
        Some f
    | _ ->
        None

  let as_machine : t -> (Id.t * Machine.t list) option = function
    | Machine (i, m) ->
        Some (i, m)
    | _ ->
        None
end

type t = (Top.t list[@sexp.list]) [@@deriving sexp]
