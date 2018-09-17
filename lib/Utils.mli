open Core

(** [iter_result] performs a computation with no result on success,
   but the possibility of signalling an error, on a list.  If any of
   the computations fails, the subsequent ones don't execute. *)
val iter_result : ('a -> (unit, 'e) result) -> 'a list -> (unit, 'e) result
