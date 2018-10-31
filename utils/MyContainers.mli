open Core

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
  (** [MyList] contains all of the monadic fold-mappable extensions we
     define in [Fold_map]. *)
  include Fold_map.S1 with type 'a t := 'a list
  include MyMonad.Extensions with type 'a t := 'a list

  (** [exclude ~f xs] is the inverse of [filter ~f xs]. *)
  val exclude : f:('a -> bool) -> 'a list -> 'a list

  (** [prefixes xs] returns all non-empty prefixes of [xs]. *)
  val prefixes : 'a list -> 'a list list
end
