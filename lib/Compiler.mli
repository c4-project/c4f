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

(** [load_and_test_specs ~path] is a wrapper around
    [CompilerSpec.load_specs] that also tests each compiler. *)
val load_and_test_specs
  :  path:string
  -> CompilerSpec.set Or_error.t
