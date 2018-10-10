open Core

(** [Extensions] outlines generic extension operations on
   Core containers. *)
module type Extensions = sig
  type 'a t

  (** [iter_result] performs a computation with no result on success,
     but the possibility of signalling an error, on a list.  If any of
     the computations fails, the subsequent ones don't execute. *)
  val iter_result : ('a -> (unit, 'e) result) -> 'a t -> (unit, 'e) result

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
module Extend : functor (S : Container.S1) -> (Extensions with type 'a t = 'a S.t)

module MyArray : Extensions with type 'a t = 'a array

(** [MyList] contains various extensions to [List]. *)
module MyList : sig
  include Extensions with type 'a t = 'a list
  include MyMonad.Extensions with type 'a t := 'a list

  (** [exclude ~f xs] is the inverse of [filter ~f xs]. *)
  val exclude : f:('a -> bool) -> 'a list -> 'a list

  (** [right_pad ~padding xs] pads every list in xs with [padding],
      ensuring all lists have equal length. *)
  val right_pad : padding:'a -> 'a list list -> 'a list list
end
