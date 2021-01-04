(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
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
      | Param of C4f_common.Id.t * int
      | Flag of C4f_common.Id.t * Flag_value.t
    [@@deriving sexp]

    include Pretty_printer.S with type t := t
  end

  type t = Action of C4f_common.Id.t * int option | Set of Setter.t
  [@@deriving sexp]

  include Pretty_printer.S with type t := t
end

(** Items at the top level *)
module Top : sig
  type t = Fuzz of Fuzz.t list  (** A fuzzer config block. *)
  [@@deriving sexp]

  include Pretty_printer.S with type t := t

  val as_fuzz : t -> Fuzz.t list option
  (** [as_fuzz top] is [Some f] if [top] is [Fuzz f], and [None] otherwise. *)
end

(** A config AST is a list of top-level items. *)
type t = Top.t list [@@deriving sexp]

(** We can pretty-print config ASTs. *)
include Pretty_printer.S with type t := t
