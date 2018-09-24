open Core

(** [ContainerExtensions] outlines generic extension operations on
   Core containers. *)
module type ContainerExtensions = sig
  type 'a cont

  (** [iter_result] performs a computation with no result on success,
     but the possibility of signalling an error, on a list.  If any of
     the computations fails, the subsequent ones don't execute. *)
  val iter_result : ('a -> (unit, 'e) result) -> 'a cont -> (unit, 'e) result
end

(** We produce a functor for extending any Core [Container] with
   [ContainerExtensions]. *)
module ContainerExtend : functor (S : Container.S1) -> (ContainerExtensions with type 'a cont = 'a S.t)

module MyArray : (ContainerExtensions with type 'a cont = 'a array)

module MyList : (ContainerExtensions with type 'a cont = 'a list)
