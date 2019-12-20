(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Act_common

(** Abstract syntax tree for config files

    The config {{!Config_parser} parser} emits this AST; later on, the
    {{!Config.File} config module proper} uses it to build a raw config. *)

open Base

(** Items in a fuzzer stanza *)
module Fuzz : sig
  type t = Action of Id.t * int option [@@deriving sexp]

  include Pretty_printer.S with type t := t
end

(** Items in a simulator stanza *)
module Backend : sig
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

  val as_user : t -> string option

  val as_host : t -> string option

  val as_copy_to : t -> string option
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
    | Backend of Id.t * Backend.t list
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
    type t = Arch | Compiler | Machine | Backend [@@deriving sexp]

    include Act_utils.Enum_types.Extension_table with type t := t
  end

  type t = Try of Category.t * Id.t [@@deriving sexp]

  include Pretty_printer.S with type t := t
end

(** Items at the top level *)
module Top : sig
  type t =
    | Default of Default.t list  (** A default resolution config block. *)
    | Fuzz of Fuzz.t list  (** A fuzzer config block. *)
    | Machine of Id.t * Machine.t list  (** A machine config block. *)
  [@@deriving sexp]

  include Pretty_printer.S with type t := t

  val as_default : t -> Default.t list option
  (** [as_default top] is [Some c] if [top] is [Default c], and [None]
      otherwise. *)

  val as_fuzz : t -> Fuzz.t list option
  (** [as_fuzz top] is [Some f] if [top] is [Fuzz f], and [None] otherwise. *)

  val as_machine : t -> (Id.t * Machine.t list) option
  (** [as_machine top] is [Some (i, m)] if [top] is [Machine (i, m)], and
      [None] otherwise. *)
end

type t = Top.t list [@@deriving sexp]
(** A config AST is a list of top-level items. *)

include Pretty_printer.S with type t := t
(** We can pretty-print config ASTs. *)
