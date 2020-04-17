(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Fetch-style read-modify-write operations. *)


(** {1 Fetch postfix operations}

    The c-mini representation of fetch-and-X instructions treats each a
    variant of the same 'fetch' instruction, with the X disambiguated by
    an extra [Post_op.t] parameter. *)
module Post_op : sig

  (** The enumeration of fetch postfix operations. *)
  type t =
    | Add (** Fetch and add. *)
    | Sub (** Fetch and subtract. *)

  include Act_utils.Enum_types.S_table with type t := t

  include Act_utils.Enum_types.Extension_table with type t := t

end

(** {1 Fetches proper} *)

type 'e t [@@deriving sexp, compare, equal]
(** Opaque type of fetches.

    Fetches take as a type parameter the expression type.  This is necessary to
    bust a dependency cycle when fetches appear in expression position. *)

    (** {2 Constructors} *)

val make :
     obj: Address.t
  -> arg: 'e
  -> mo: Mem_order.t
  -> op: Post_op.t
  -> 'e t
(** [make ~obj ~arg ~mo ~op] makes a fetch-and-X operation, whereby X is
    given by [op]; [obj] is the address of the atomic object to fetch;
    [arg] is the argument to [op]; and [mo] the memory order. *)

    (** {2 Accessors} *)

val obj: _ t -> Address.t
(** [obj f] gets the address of the atomic object [f] is fetching. *)

val arg: 'e t -> 'e
(** [arg f] gets the argument to [f]'s operation. *)

val mo: _ t -> Mem_order.t
(** [mo f] gets [f]'s memory order. *)

val op: _ t -> Post_op.t
(** [op f] gets [f]'s postfix operation. *)
