open Core

(** [Extensions] outlines generic extension operations on
   Core containers. *)
module type Extensions = sig
  type 'a t

  (** [max_measure ~measure ?default xs] measures each item in [xs]
     according to [measure], and returns the highest measure reported.
     If [xs] is empty, return [default] if given, and [0]
     otherwise. *)
  val max_measure
    :  measure:('a -> int)
    -> ?default:int
    -> 'a t
    -> int
end

(** We produce a functor for extending any Core [Container] with
   [Extensions]. *)
module Extend : functor (S : Container.S1) -> Extensions with type 'a t := 'a S.t

module MyArray : Extensions with type 'a t := 'a array

(** [partial_order] is the type of returns from [partial_compare]. *)
type partial_order =
  [ `Equal
  | `Subset
  | `Superset
  | `NoOrder
  ] [@@deriving sexp]
;;

(** [SetExtensions] contains various extensions to implementations of
   [Set.S]. *)
module type SetExtensions = sig
  type t
  (** [disjoint x y] returns [true] provided that [x] and [y] have no
      elements in common. *)
  val disjoint : t -> t -> bool

  (** [partial_compare x y] compares two sets [x] and [y] by analysing
      their symmetric difference. *)
  val partial_compare : t -> t -> partial_order
end

(** [SetExtend] builds set extensions for module [S]. *)
module SetExtend : functor (S : Set.S) -> SetExtensions with type t := S.t;;

(** [MyList] contains various extensions to [List]. *)
module MyList : sig
  include Extensions with type 'a t := 'a list
  include MyMonad.Extensions with type 'a t := 'a list

  (** [exclude ~f xs] is the inverse of [filter ~f xs]. *)
  val exclude : f:('a -> bool) -> 'a list -> 'a list

  (** [right_pad ~padding xs] pads every list in xs with [padding],
      ensuring all lists have equal length. *)
  val right_pad : padding:'a -> 'a list list -> 'a list list

  (** [prefixes xs] returns all non-empty prefixes of [xs]. *)
  val prefixes : 'a list -> 'a list list
end
