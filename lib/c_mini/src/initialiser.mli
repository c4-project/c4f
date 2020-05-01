(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Mini-model: initialisers. *)

(** Opaque type of initialisers. *)
type t [@@deriving sexp, compare, equal, quickcheck]

(** {1 Constructors} *)

val make : ty:Type.t -> ?value:Constant.t -> unit -> t
(** [make ~ty ?value ()] makes an initialiser with type [ty] and optional
    initialised value [value]. *)

(** {2 Shortcuts} *)

val of_int : ?is_atomic:bool -> ?is_volatile:bool -> int -> t
(** [of_int ?is_atomic ?is_volatile value] makes an initialiser for an
    integer with value [value]. If [is_atomic] is present and true, the type
    will be atomic_int; otherwise, it will be int. If [is_volatile] is
    present and true, the type will be volatile. *)

(** {1 Accessors} *)

val ty : t -> Type.t
(** [ty init] gets the type of [init]. *)

val value : t -> Constant.t option
(** [value init] gets the initialised value of [init], if it has one. *)
