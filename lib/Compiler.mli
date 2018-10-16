open Core

(** [Intf] is the interface of compiler modules. *)
module type Intf = sig
  (** [id] gets the identifier for this compiler. *)
  val id : CompilerSpec.Id.t

  (** [test ()] tests that the compiler is working. *)
  val test : unit -> unit Or_error.t

  (** [compile ~infile ~outfile] runs the compiler on [infile],
      emitting assembly to [outfile] and returning any errors that arise. *)
  val compile : infile:string -> outfile:string -> unit Or_error.t
end

(** [from_spec cid spec] generates a compiler module from [cid] and [spec]. *)
val from_spec
  :  CompilerSpec.Id.t
  -> CompilerSpec.t
  -> (module Intf)

(** [test_specs specs] tests all enabled specs in the set [specs], and
    partitions them into a list of valid specs and test failures. *)
val test_specs : CompilerSpec.Set.t -> (CompilerSpec.Set.t * Error.t list)
