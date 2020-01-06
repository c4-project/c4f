(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Abstract syntax tree for config files

    The config {{!Parser} parser} emits this AST; later on, the {{!Global}
    config module proper} uses it to build a raw config. *)

open Base

(** Items in a fuzzer stanza *)
module Fuzz : sig
  (** Values on Boolean flags. *)
  module Flag_value : sig
    (** Type of flag value AST nodes. *)
    type t = Ratio of int * int | Exact of bool [@@deriving sexp]

    include Pretty_printer.S with type t := t
  end

  (** Bodies of 'set' directives in fuzz configurations. *)
  module Setter : sig
    (** Type of property setter AST nodes. *)
    type t =
      | Param of Act_common.Id.t * int
      | Flag of Act_common.Id.t * Flag_value.t
    [@@deriving sexp]

    include Pretty_printer.S with type t := t
  end

  type t = Action of Act_common.Id.t * int option | Set of Setter.t
  [@@deriving sexp]

  include Pretty_printer.S with type t := t
end

(** Items in a backend stanza *)
module Backend : sig
  type t =
    | Cmd of string
    | Style of Act_common.Id.t
    | C_model of string
    | Asm_model of Act_common.Id.t * string
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
    | Style of Act_common.Id.t
    | Emits of Act_common.Id.t
    | Cmd of string
    | Argv of string list
  [@@deriving sexp]

  include Pretty_printer.S with type t := t
end

(** Items in a machine stanza *)
module Machine : sig
  (** Type of machine stanza items. *)
  type t =
    | Compiler of Act_common.Id.t * Compiler.t list
        (** A compiler on this particular machine. *)
    | Enabled of bool  (** Whether or not the machine is enabled. *)
    | Via of Via.t  (** Where the machine is located. *)
    | Backend of Act_common.Id.t * Backend.t list
        (** Information about how to run a particular backend on this
            machine. *)
  [@@deriving sexp]

  include Pretty_printer.S with type t := t

  val as_compiler : t -> (Act_common.Id.t * Compiler.t list) option
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

  type t = Try of Category.t * Act_common.Id.t [@@deriving sexp]

  include Pretty_printer.S with type t := t
end

(** Items at the top level *)
module Top : sig
  type t =
    | Default of Default.t list  (** A default resolution config block. *)
    | Fuzz of Fuzz.t list  (** A fuzzer config block. *)
    | Machine of Act_common.Id.t * Machine.t list
        (** A machine config block. *)
  [@@deriving sexp]

  include Pretty_printer.S with type t := t

  val as_default : t -> Default.t list option
  (** [as_default top] is [Some c] if [top] is [Default c], and [None]
      otherwise. *)

  val as_fuzz : t -> Fuzz.t list option
  (** [as_fuzz top] is [Some f] if [top] is [Fuzz f], and [None] otherwise. *)

  val as_machine : t -> (Act_common.Id.t * Machine.t list) option
  (** [as_machine top] is [Some (i, m)] if [top] is [Machine (i, m)], and
      [None] otherwise. *)
end

type t = Top.t list [@@deriving sexp]
(** A config AST is a list of top-level items. *)

include Pretty_printer.S with type t := t
(** We can pretty-print config ASTs. *)
