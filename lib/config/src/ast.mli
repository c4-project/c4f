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

open Act_common

(** Abstract syntax tree for config files

    The config {{!Config_parser} parser} emits this AST; later on, the
    {{!Config.File} config module proper} uses it to build a raw config. *)

open Base

(** Items in a C-preprocessor stanza *)
module Cpp : sig
  type t = Cmd of string | Argv of string list | Enabled of bool
  [@@deriving sexp]

  include Pretty_printer.S with type t := t
end

(** Items in a fuzzer stanza *)
module Fuzz : sig
  type t = Action of Id.t * int option [@@deriving sexp]

  include Pretty_printer.S with type t := t
end

(** Items in a simulator stanza *)
module Sim : sig
  type t =
    | Cmd of string
    | Style of Id.t
    | C_model of string
    | Asm_model of Id.t * string
  [@@deriving sexp]

  include Pretty_printer.S with type t := t
end

(** Items in a SSH stanza *)
module Ssh : sig
  type t = User of string | Host of string | Copy_to of string
  [@@deriving sexp]

  include Pretty_printer.S with type t := t
end

(** Items in a via stanza *)
module Via : sig
  type t = Local | Ssh of Ssh.t list [@@deriving sexp]

  include Pretty_printer.S with type t := t
end

(** Items in a compiler stanza *)
module Compiler : sig
  type t =
    | Enabled of bool
    | Style of Id.t
    | Emits of Id.t
    | Cmd of string
    | Argv of string list
  [@@deriving sexp]

  include Pretty_printer.S with type t := t
end

(** Items in a machine stanza *)
module Machine : sig
  (** Type of machine stanza items. *)
  type t =
    | Compiler of Id.t * Compiler.t list
        (** A compiler on this particular machine. *)
    | Enabled of bool  (** Whether or not the machine is enabled. *)
    | Via of Via.t  (** Where the machine is located. *)
    | Sim of Id.t * Sim.t list
        (** Information about how to run a particular simulator on this
            machine. *)
  [@@deriving sexp]

  include Pretty_printer.S with type t := t

  val as_compiler : t -> (Id.t * Compiler.t list) option
  (** [as_compiler top] is [Some (i, c)] if [top] is [Compiler (i, c)], and
      [None] otherwise. *)
end

(** Items in a default stanza *)
module Default : sig
  (** Categories of default item *)
  module Category : sig
    type t = Arch | Compiler | Machine | Sim [@@deriving sexp]

    include Act_utils.Enum.Extension_table with type t := t
  end

  type t = Try of Category.t * Id.t [@@deriving sexp]

  include Pretty_printer.S with type t := t
end

(** Items at the top level *)
module Top : sig
  type t =
    | Cpp of Cpp.t list  (** A C preprocessor config block. *)
    | Default of Default.t list  (** A default resolution config block. *)
    | Fuzz of Fuzz.t list  (** A fuzzer config block. *)
    | Machine of Id.t * Machine.t list  (** A machine config block. *)
  [@@deriving sexp]

  include Pretty_printer.S with type t := t

  val as_cpp : t -> Cpp.t list option
  (** [as_cpp top] is [Some c] if [top] is [Cpp c], and [None] otherwise. *)

  val as_default : t -> Default.t list option
  (** [as_default top] is [Some c] if [top] is [Default c], and [None]
      otherwise. *)

  val as_fuzz : t -> Fuzz.t list option
  (** [as_fuzz top] is [Some f] if [top] is [Fuzz f], and [None] otherwise. *)

  val as_machine : t -> (Id.t * Machine.t list) option
  (** [as_machine top] is [Some (i, m)] if [top] is [Machine (i, m)], and
      [None] otherwise. *)
end

(** A config AST is a list of top-level items. *)
type t = Top.t list [@@deriving sexp]
