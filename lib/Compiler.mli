open Core

(** [compiler id spec ps] runs the compiler with id [id] and spec [spec]
    using the paths in pathset [ps]. *)
val compile : CompilerSpec.Id.t -> CompilerSpec.t -> Pathset.t -> unit Or_error.t

(** [test spec] checks whether the compiler with spec [spec] exists and is
    sensible. *)
val test : CompilerSpec.t -> unit Or_error.t

(** [load_and_test_specs ~path] is a wrapper around
    [CompilerSpec.load_specs] that also tests each compiler. *)
val load_and_test_specs
  :  path:string
  -> CompilerSpec.set Or_error.t
